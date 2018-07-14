{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
module Language.Malgo.BackEnd.LLVM where

import           System.Environment        (lookupEnv)
import           Control.Lens.TH
import           Data.Text.Prettyprint.Doc
import           Language.Malgo.ID
import           Language.Malgo.IR.IR hiding (prims)
import qualified LLVM.AST
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.Type                   as LT
import           LLVM.IRBuilder                  as IRBuilder
import           RIO
import qualified RIO.Char                        as Char
import qualified RIO.Map                         as Map
import qualified RIO.Text                        as Text
import           System.Exit

data GenState =
  GenState { _table      :: Map (ID MType) O.Operand
           , _terminator :: O.Operand -> GenExpr ()
           , _internal   :: Map Text O.Operand
           , _prims      :: IORef (Map Text O.Operand)
           , _logFunc    :: LogFunc
           }
type GenExpr a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (RIO GenState)

makeLenses ''GenState

dumpLLVM :: MonadIO m => GenDec a -> m [LLVM.AST.Definition]
dumpLLVM m = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  lo <- logOptionsHandle stderr verbose
  p <- newIORef Map.empty
  withLogFunc lo $ \lf -> do
    let genState = GenState Map.empty ret Map.empty p lf
    runRIO genState (execModuleBuilderT emptyModuleBuilder m)

instance HasLogFunc GenState where
  logFuncL = logFunc

addTable :: MonadReader GenState m => ID MType -> O.Operand -> m a -> m a
addTable name opr m =
  local (over table (Map.insert name opr)) m

addInternal :: MonadReader GenState m => Text -> O.Operand -> m a -> m a
addInternal name opr m =
  local (over internal (Map.insert name opr)) m

convertType :: MType -> LT.Type
convertType (IntTy i) = LT.IntegerType (fromInteger i)
convertType DoubleTy = LT.double
convertType (PointerTy t) = LT.ptr $ convertType t
convertType (StructTy xs) = LT.StructureType False (map convertType xs)
convertType (FunctionTy retTy params) = LT.ptr $ LT.FunctionType (convertType retTy) (map convertType params) False

getRef :: (MonadIO m, MonadReader GenState m) => ID MType -> m O.Operand
getRef i = do
  m <- view table
  case Map.lookup i m of
    Just x -> return x
    Nothing -> do liftRIO $ logError (displayShow i <> " is not found in " <> displayShow m)
                  liftIO exitFailure

term :: IRBuilderT GenDec O.Operand -> IRBuilderT GenDec ()
term o = do
  t <- view terminator
  o' <- o
  t o'

char :: Monad m => Integer -> m O.Operand
char = return . O.ConstantOperand . C.Int 8

sizeof :: MonadIRBuilder m => LT.Type -> m O.Operand
sizeof ty = do
  nullptr <- pure $ O.ConstantOperand (C.Null (LT.ptr ty))
  ptr <- gep nullptr [O.ConstantOperand (C.Int 32 1)]
  ptrtoint ptr LT.i64

gcMalloc :: (MonadIO m, MonadIRBuilder m, MonadReader GenState m) => O.Operand -> m O.Operand
gcMalloc bytesOpr = do
  f <- Map.lookup "GC_malloc" <$> view internal
  case f of
    Just f' -> call f' [(bytesOpr, [])]
    Nothing -> do liftRIO $ logError "unreachable(gcMalloc)"
                  liftIO exitFailure

malloc :: (MonadReader GenState m, MonadIRBuilder m, MonadIO m) => LT.Type -> m O.Operand
malloc ty = do
  p <- gcMalloc =<< sizeof ty
  bitcast p (LT.ptr ty)

genExpr :: Expr (ID MType) -> IRBuilderT GenDec ()
genExpr e = term (genExpr' e) -- `named` "x"

genExpr' :: Expr (ID MType) -> IRBuilderT GenDec O.Operand
genExpr' (Var a) = getRef a
genExpr' (Int i) = int32 i
genExpr' (Float d) = double d
genExpr' (Bool b) = bit (if b then 1 else 0)
genExpr' (Char c) = char (toInteger $ Char.ord c)
genExpr' (String xs) = do
  p <- gcMalloc (O.ConstantOperand $ C.Int 64 $ toInteger $ Text.length xs + 1)
  mapM_ (addChar p) (zip [0..] $ Text.unpack xs <> ['\0'])
  return p
  where addChar p (i, c) = do
          i' <- int32 i
          p' <- gep p [i']
          c' <- char (toInteger $ Char.ord c)
          store p' 0 c'
genExpr' Unit = return (O.ConstantOperand $ C.Undef (LT.StructureType False []))
genExpr' (Prim orig ty) = do
  ps <- view prims
  psMap <- readIORef ps
  case Map.lookup orig psMap of
    Just p -> return p
    Nothing -> do (argtys, retty) <-
                    case ty of
                      (FunctionTy r p) -> return (map convertType p, convertType r)
                      _ -> do liftRIO $ logError "extern symbol must have a function type"
                              liftIO exitFailure
                  p <- lift $ extern (fromString $ Text.unpack orig) argtys retty
                  liftIO $ modifyIORef ps (Map.insert orig p)
                  return p
genExpr' (Tuple xs) = do
  p <- malloc (LT.StructureType False (map (convertType . mTypeOf) xs))
  forM_ (zip [0..] xs) $ \(i, x) -> do
    p' <- gep p [ O.ConstantOperand (C.Int 32 0), O.ConstantOperand (C.Int 32 i)]
    o <- getRef x
    store p' 0 o
  return p
genExpr' (Apply f args) = do
  f' <- getRef f
  args' <- mapM (getRef >=> return . (, [])) args
  call f' args'
genExpr' (Let name val body) = do
  val' <- genExpr' val
  addTable name val' (genExpr' body)
genExpr' LetRec{} = do
  liftRIO $ logError "unreachable(LetRec)"
  liftIO exitFailure
genExpr' (Cast ty a) = do
  a' <- getRef a
  bitcast a' (convertType ty)
genExpr' (Access a is) = do
  a' <- getRef a
  p <- gep a' (map (O.ConstantOperand . C.Int 32 . toInteger) is)
  load p 0
genExpr' (If c t f) = do
  c' <- getRef c
  r <- alloca (convertType (mTypeOf t)) Nothing 0
  end <- freshName "end"
  tLabel <- freshName "then"
  fLabel <- freshName "else"
  condBr c' tLabel fLabel
  local (set terminator (\o -> store r 0 o >> br end)) $ do
    emitBlockStart tLabel; genExpr t
    emitBlockStart fLabel; genExpr f
  emitBlockStart end
  load r 0

genDefn :: Defn (ID MType) -> GenDec ()
genDefn (DefFun fn params body) = do
  let fn' = fromString $ show $ pretty fn
  let params' = map (\(ID name _ ty) ->
                       (convertType ty, fromString $ show $ pretty name)) params
  let retty' = convertType (mTypeOf body)
  void $ function fn' params' retty'
    $ \xs -> local (over table (Map.fromList ((fn, fnopr) : zip params xs) <>))
    $ genExpr body
  where fnopr = O.ConstantOperand $ C.GlobalReference (convertType (mTypeOf fn)) (fromString $ show $ pretty fn)
        convertType' (FunctionTy r p) = LT.FunctionType (convertType r) (map convertType p) False
        convertType' _ = error "unreachable"


genProgram :: Program (ID MType) -> GenDec ()
genProgram (Program m defs) = do
  a <- extern "malloc_gc" [LT.i64] (LT.ptr LT.i8)
  addInternal "GC_malloc" a $ do
    gcInit <- extern "init_gc" [] LT.void
    local (over table (Map.fromList (zip (map (\(DefFun f _ _) -> f) defs) defs') <>)) $ do
      mapM_ genDefn defs
      void $ function "main" [] LT.i32
        (\_ -> do void $ call gcInit []
                  m' <- getRef m
                  void $ call m' []
                  ret =<< int32 0)
  where defs' = map (\(DefFun fn _ _) ->
                       O.ConstantOperand $ C.GlobalReference (convertType (mTypeOf fn)) (fromString $ show $ pretty fn)) defs
        convertType' (FunctionTy r p) = LT.FunctionType (convertType r) (map convertType p) False
        convertType' _ = error "unreachable"

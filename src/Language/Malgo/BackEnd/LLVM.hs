{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
module Language.Malgo.BackEnd.LLVM
  ( dumpLLVM
  , genExpr
  , genDefn
  , genProgram
  ) where

import           Control.Lens                 hiding (ix)
import qualified Data.Char                    as Char
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as Text
import           Language.Malgo.ID
import           Language.Malgo.IR.IR         hiding (prims)
import qualified Language.Malgo.Pretty        as P
import           Language.Malgo.TypeRep.MType
import qualified LLVM.AST
import qualified LLVM.AST.Constant            as C
import qualified LLVM.AST.Operand             as O
import qualified LLVM.AST.Type                as LT
import           LLVM.IRBuilder               as IRBuilder
import           Relude

data GenState =
  GenState { _table      :: Map (ID MType) O.Operand
           , _terminator :: O.Operand -> GenExpr ()
           , _internal   :: Map Text O.Operand
           , _prims      :: IORef (Map Text O.Operand)
           }
type GenExpr a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (ReaderT GenState IO)

makeLenses ''GenState

dumpLLVM :: MonadIO m => GenDec a -> m [LLVM.AST.Definition]
dumpLLVM m = liftIO $ do
  p <- newIORef Map.empty
  let genState = GenState Map.empty ret Map.empty p
  liftIO $ usingReaderT genState $ execModuleBuilderT emptyModuleBuilder m

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
    Just x  -> return x
    Nothing -> error $ show i <> " is not found in " <> show m

term :: IRBuilderT GenDec O.Operand -> IRBuilderT GenDec ()
term o = do
  t <- view terminator
  o' <- o
  t o'

char :: Monad m => Integer -> m O.Operand
char = return . O.ConstantOperand . C.Int 8

sizeof :: (MonadModuleBuilder m, MonadIRBuilder m) => LT.Type -> m O.Operand
sizeof ty = do
  let nullptr = O.ConstantOperand (C.Null (LT.ptr ty))
  ptr <- gep nullptr [O.ConstantOperand (C.Int 64 1)]
  ptrtoint ptr LT.i64

gcMalloc :: ( MonadIO m
            , MonadIRBuilder m
            , MonadReader GenState m)
         => O.Operand -> m O.Operand
gcMalloc bytesOpr = do
  f <- Map.lookup "GC_malloc" <$> view internal
  case f of
    Just f' -> call f' [(bytesOpr, [])]
    Nothing -> error "unreachable(gcMalloc)"

malloc :: ( MonadModuleBuilder m
          , MonadReader GenState m
          , MonadIRBuilder m
          , MonadIO m)
       => LT.Type -> m O.Operand
malloc ty = do
  p <- gcMalloc =<< sizeof ty
  bitcast p (LT.ptr ty)

genExpr :: Expr (ID MType) -> IRBuilderT GenDec ()
genExpr e = term (genExpr' e)

genExpr' :: Expr (ID MType) -> IRBuilderT GenDec O.Operand
genExpr' (Var a) = getRef a
genExpr' (Int i) = pure $ int64 i
genExpr' (Float d) = pure $ double d
genExpr' (Bool b) = pure $ bit (if b then 1 else 0)
genExpr' (Char c) = char (toInteger $ Char.ord c)
genExpr' (String xs) = do
  p <- gcMalloc (O.ConstantOperand $ C.Int 64
                 $ toInteger $ Text.length xs + 1)
  mapM_ (addChar p) (zip [0..] $ toString xs <> ['\0'])
  return p
  where addChar p (i, c) = do
          p' <- gep p [int64 i]
          c' <- char (toInteger $ Char.ord c)
          store p' 0 c'
genExpr' Unit =
  return (O.ConstantOperand $ C.Undef $ LT.StructureType False [])
genExpr' (Prim orig ty) = do
  ps <- view prims
  psMap <- readIORef ps
  case Map.lookup orig psMap of
    Just p -> return p
    Nothing -> do (argtys, retty) <-
                    case ty of
                      (FunctionTy r p) -> return (map convertType p, convertType r)
                      _ -> error "extern symbol must have a function type"
                  p <- lift $ extern (fromString $ toString orig) argtys retty
                  modifyIORef ps (Map.insert orig p)
                  return p
genExpr' (Tuple xs) = do
  p <- malloc (LT.StructureType False (map (convertType . mTypeOf) xs))
  forM_ (zip [0..] xs) $ \(i, x) -> do
    p' <- gep p [ O.ConstantOperand (C.Int 32 0), O.ConstantOperand (C.Int 32 i)]
    o <- getRef x
    store p' 0 o
  return p
genExpr' (MakeArray ty size) = do
  size' <- getRef size
  byteSize <- mul size' =<< sizeof (convertType ty)
  arr <- gcMalloc byteSize
  bitcast arr (LT.ptr $ convertType ty)
genExpr' (Read arr ix) = do
  arr' <- getRef arr
  ix' <- getRef ix
  p <- gep arr' [ix']
  load p 0
genExpr' (Write arr ix val) = do
  arr' <- getRef arr
  ix' <- getRef ix
  val' <- getRef val
  p <- gep arr' [ix']
  store p 0 val'
  return (O.ConstantOperand $ C.Undef $ LT.StructureType False [])
genExpr' (Apply f args) = do
  f' <- getRef f
  args' <- mapM (getRef >=> return . (, [])) args
  call f' args'
genExpr' (Let name val body) = do
  val' <- genExpr' val `named` fromString (show (P.pPrint name))
  local (over table (Map.insert name val')) (genExpr' body)
genExpr' LetRec{} =
  error "unreachable(LetRec)"
genExpr' (Cast ty a) = do
  a' <- getRef a
  bitcast a' (convertType ty)
genExpr' (Access a is) = do
  a' <- getRef a
  p <- gep a' (map (O.ConstantOperand . C.Int 32 . toInteger) is)
  load p 0
genExpr' (Store a is v) = do
  a' <- getRef a
  v' <- getRef v
  p <- gep a' (map (O.ConstantOperand . C.Int 32 . toInteger) is)
  store p 0 v'
  genExpr' Unit
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
  let fn' = fromString $ show $ P.pPrint fn
  let params' = map (\(ID name _ ty) ->
                       (convertType ty, fromString $ show $ P.pPrint name)) params
  let retty' = convertType (mTypeOf body)
  void $ function fn' params' retty'
    $ \xs -> local (over table (Map.fromList ((fn, fnopr) : zip params xs) <>))
             $ genExpr body
  where fnopr = O.ConstantOperand $ C.GlobalReference (convertType (mTypeOf fn)) (fromString $ show $ P.pPrint fn)

genProgram :: Program (ID MType) -> GenDec ()
genProgram (Program m defs) = do
  a <- extern "malloc_gc" [LT.i64] (LT.ptr LT.i8)
  local (over internal (Map.insert "GC_malloc" a)) $ do
    gcInit <- extern "init_gc" [] LT.void
    local (over table (Map.fromList (zip (map (\(DefFun f _ _) -> f) defs) defs') <>)) $ do
      mapM_ genDefn defs
      void $ function "main" [] LT.i32
        (\_ -> do void $ call gcInit []
                  m' <- getRef m
                  void $ call m' []
                  ret (int32 0))
  where defs' = map (\(DefFun fn _ _) ->
                       O.ConstantOperand $ C.GlobalReference (convertType (mTypeOf fn)) (fromString $ show $ P.pPrint fn)) defs

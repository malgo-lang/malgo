{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
module Language.Malgo.BackEnd.New.LLVM ( GenLLVM ) where

import qualified Data.ByteString             as B
import qualified Data.Map                    as Map
import           Language.Malgo.ID
import           Language.Malgo.IR.HIR       (Lit (..))
import           Language.Malgo.IR.LIR
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import qualified LLVM.AST
import qualified LLVM.AST.Constant           as C
import qualified LLVM.AST.Operand            as O
import qualified LLVM.AST.Type               as LT
import           LLVM.IRBuilder              as IRBuilder
import           Relude                      hiding (Type)
import           Relude.Extra.Map            hiding (size)

data GenLLVM

instance Pass GenLLVM (Program Type TypedID) [LLVM.AST.Definition] where
  isDump _ = False -- TODO: support dump llvm-ir ast
  trans Program{ functions, mainExpr } = dumpLLVM $ mdo
    funMap <- local (\st -> st { functionMap = funMap }) $ genFunctions functions
    local (\st -> st { functionMap = funMap }) $ void $ function "main" [] LT.i32 $ \_ -> do
      void $ genExpr mainExpr
      ret (int32 0)

data GenState =
  GenState { variableMap :: Map TypedID O.Operand
           , functionMap :: Map TypedID O.Operand
           , terminator  :: O.Operand -> GenExpr ()
           , captures    :: O.Operand
           , prims       :: IORef (Map Text O.Operand)
           }

type GenExpr a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (ReaderT GenState MalgoM)

dumpLLVM :: MonadIO m => ModuleBuilderT (ReaderT GenState m) a -> m [LLVM.AST.Definition]
dumpLLVM m = do
  p <- newIORef mempty
  let genState = GenState { variableMap = mempty
                          , functionMap = mempty
                          , terminator = ret
                          , captures = undef (LT.ptr LT.i8)
                          , prims = p
                          }
  usingReaderT genState $ execModuleBuilderT emptyModuleBuilder m

convertType :: Type -> LT.Type
convertType (TyApp FunC (r:ps)) =
  LT.ptr $ LT.StructureType False [LT.ptr $ LT.FunctionType (convertType r) (LT.ptr LT.i8 : map convertType ps) False, LT.ptr LT.i8]
convertType (TyApp IntC []) = LT.i64
convertType (TyApp FloatC []) = LT.double
convertType (TyApp BoolC []) = LT.i1
convertType (TyApp CharC []) = LT.i8
convertType (TyApp StringC []) = LT.ptr LT.i8
convertType (TyApp TupleC xs) = LT.ptr (LT.StructureType False (map convertType xs))
convertType (TyApp ArrayC [x]) = LT.ptr (convertType x)
convertType t = error $ fromString $ "unreachable(convertType): " <> dumpStr t

functionType :: (HasType a1, HasType a2) => [a2] -> a1 -> LT.Type
functionType ps r = LT.ptr $ LT.FunctionType (convertType $ typeOf r) (LT.ptr LT.i8 : map (convertType . typeOf) ps) False

getVar :: MonadReader GenState m => ID Type -> m O.Operand
getVar i = do
  m <- asks variableMap
  case lookup i m of
    Just x  -> pure x
    Nothing -> error $ show i <> " is not found in " <> show m

getFun :: MonadReader GenState m => ID Type -> m O.Operand
getFun i = do
  m <- asks functionMap
  case lookup i m of
    Just x  -> pure x
    Nothing -> error $ show i <> " is not found in " <> show m

term :: GenExpr O.Operand -> GenExpr ()
term o =
  asks terminator >>= \t ->
    t =<< o

mallocBytes :: (MonadModuleBuilder m, MonadIRBuilder m) =>
                 O.Operand -> m O.Operand
mallocBytes bytesOpr = do
  f <- extern "GC_malloc" [LT.i64] (LT.ptr LT.i8)
  call f [(bytesOpr, [])]

mallocType :: (MonadModuleBuilder m, MonadIRBuilder m) =>
                LT.Type -> m O.Operand
mallocType ty = do
  p <- mallocBytes (sizeof ty)
  bitcast p (LT.ptr ty)

sizeof :: LT.Type -> O.Operand
sizeof = O.ConstantOperand . C.sizeof

undef :: LT.Type -> O.Operand
undef ty = O.ConstantOperand $ C.Undef ty

genFunctions = undefined

genTermExpr :: Expr Type (ID Type) -> GenExpr ()
genTermExpr e = term (genExpr e)

genExpr :: Expr Type (ID Type) -> GenExpr O.Operand
genExpr (Var a)       = getVar a
genExpr (Lit (Int x)) = pure $ int64 x
genExpr (Lit (Float x)) = pure $ double x
genExpr (Lit (Bool True)) = pure $ bit 1
genExpr (Lit (Bool False)) = pure $ bit 0
genExpr (Lit (Char x)) = pure $ int8 $ toInteger $ ord x
genExpr (Lit (String x)) = do
  let bytes = B.unpack $ encodeUtf8 @Text @ByteString x
  n <- fresh
  global n (LT.ArrayType (toEnum $ length bytes + 1) LT.i8) (C.Array LT.i8 $ map (C.Int 8 . toInteger) bytes)
genExpr (Tuple xs) = do
  tuplePtr <- mallocType (LT.StructureType False (map (convertType . typeOf) xs))
  forM_ (zip [0..] xs) $ \(i, x) -> do
    elemPtr <- gep tuplePtr [int32 0, int32 i]
    valOpr <- getVar x
    store elemPtr 0 valOpr
  pure tuplePtr
genExpr (TupleAccess t i) = do
  tuplePtr <- getVar t
  elemPtr <- gep tuplePtr [int32 0, int32 $ toInteger i]
  load elemPtr 0
genExpr (MakeArray ty size) = do
  sizeVal <- getVar size
  byteSize <- mul sizeVal (sizeof (convertType ty))
  arr <- mallocBytes byteSize
  bitcast arr (LT.ptr $ convertType ty)
genExpr (ArrayRead arr ix) = do
  arrOpr <- getVar arr
  ixOpr <- getVar ix
  ptr <- gep arrOpr [ixOpr]
  load ptr 0
genExpr (ArrayWrite arr ix val) = do
  arrOpr <- getVar arr
  ixOpr <- getVar ix
  valOpr <- getVar val
  ptr <- gep arrOpr [ixOpr]
  store ptr 0 valOpr
  pure $ undef $ LT.StructureType False []
genExpr (MakeClosure f cs) = do
  -- generate captures
  let capTy = LT.StructureType False (map (convertType . typeOf) cs)
  capPtr <- mallocType capTy
  forM_ (zip [0..] cs) $ \(i, c) -> do
    capElemPtr <- gep capPtr [int32 0, int32 i]
    valOpr <- getVar c
    store capElemPtr 0 valOpr

  -- generate closure
  let clsTy = convertType (typeOf f)
  clsPtr <- mallocType clsTy
  clsFunPtr <- gep clsPtr [int32 0, int32 0]
  funOpr <- getFun f
  store clsFunPtr 0 funOpr
  clsCapPtr <- gep clsPtr [int32 0, int32 1]
  store clsCapPtr 0 capPtr

  pure clsPtr
genExpr (CallDirect f args) = do
  funOpr <- getFun f
  argOprs <- genArgs (undef (LT.ptr LT.i8)) args
  call funOpr argOprs
genExpr (CallWithCaptures f args) = do
  funOpr <- getFun f
  GenState { captures } <- ask
  argOprs <- genArgs captures args
  call funOpr argOprs
genExpr (CallClosure f args) = do
  clsPtr <- getVar f
  clsFunPtr <- gep clsPtr [int32 0, int32 0]
  clsFunOpr <- load clsFunPtr 0
  clsCapPtr <- gep clsPtr [int32 0, int32 1]
  clsCapOpr <- load clsCapPtr 0
  argOprs <- genArgs clsCapOpr args
  call clsFunOpr argOprs
genExpr (Let defs e) = mdo
  defs' <- local (\st -> st {
                     variableMap = defs' <> variableMap st })
    $ fmap mconcat
    $ forM defs $ \(x, val) -> do
    valOpr <- genExpr val
    pure $ Map.fromList [(x, valOpr)]
  local (\st -> st { variableMap = defs' <> variableMap st }) $
    genExpr e
genExpr (If c t f) = mdo
  cOpr <- getVar c
  result <- alloca (convertType (typeOf t)) Nothing 0
  condBr cOpr tLabel fLabel
  (tLabel, fLabel) <- local (\st -> st { terminator = \o -> store result 0 o >> br end}) $ do
    tl <- block `named` "then"; genTermExpr t
    fl <- block `named` "else"; genTermExpr f
    pure (tl, fl)
  end <- block `named` "endif"
  load result 0

genArgs :: MonadReader GenState m => O.Operand -> [ID Type] -> m [(O.Operand, [a])]
genArgs capturesOpr xs = do
  xs' <- mapM (fmap (,[]) . getVar) xs
  pure $ (capturesOpr, []) : xs'

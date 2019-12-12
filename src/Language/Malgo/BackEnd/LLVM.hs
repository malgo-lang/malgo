{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
module Language.Malgo.BackEnd.LLVM ( GenLLVM ) where

import           Control.Exception               (assert)
import qualified Data.ByteString                 as B
import qualified Data.Map                        as Map
import           Language.Malgo.ID
import           Language.Malgo.IR.HIR           (Lit (..), Op (..))
import           Language.Malgo.IR.LIR
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import qualified LLVM.AST
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate       as IP
import qualified LLVM.AST.Operand                as O
import qualified LLVM.AST.Type                   as LT
import qualified LLVM.AST.Typed                  as LT
import           LLVM.IRBuilder                  hiding (store)
import qualified LLVM.IRBuilder                  as IRBuilder
import           Relude                          hiding (Type)
import           Relude.Extra.Map                hiding (size)

data GenLLVM

instance Pass GenLLVM (Program Type TypedID) [LLVM.AST.Definition] where
  isDump _ = False -- TODO: support dump llvm-ir ast
  trans Program{ functions, mainExpr } = dumpLLVM $ do
    let funMap = mconcat $ map genFunMap functions
    local (\st -> st { functionMap = funMap }) $ mapM_ genFunction functions
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

mallocBytes :: (MonadReader GenState m, MonadIO m,
                  MonadIRBuilder m, MonadModuleBuilder m) =>
                 O.Operand -> m O.Operand
mallocBytes bytesOpr = do
  GenState { prims } <- ask
  psMap <- readIORef prims
  case lookup "GC_malloc" psMap of
    Just f -> call f [(bytesOpr, [])]
    Nothing -> do
      f <- extern "GC_malloc" [LT.i64] (LT.ptr LT.i8)
      modifyIORef prims (Map.insert "GC_malloc" f)
      call f [(bytesOpr, [])]

mallocType :: (MonadReader GenState m, MonadIO m, MonadIRBuilder m,
                 MonadModuleBuilder m) =>
                LT.Type -> m O.Operand
mallocType ty = do
  p <- mallocBytes (sizeof ty)
  bitcast p (LT.ptr ty)

genFunMap :: Func Type TypedID -> Map TypedID O.Operand
genFunMap Func{ name, captures, params = _, body = _ } =
  let TyFun ps r = typeOf name
  in Map.singleton name
     $ O.ConstantOperand
     $ C.GlobalReference (functionType (isNothing captures) ps r)
     (fromString $ show $ pPrint name)

functionType :: (HasType a1, HasType a2) => Bool -> [a2] -> a1 -> LT.Type
functionType isKnown ps r = LT.ptr $ LT.FunctionType
  { LT.resultType = convertType $ typeOf r
  , LT.argumentTypes = (if isKnown then id else (LT.ptr LT.i8 :)) $ map (convertType . typeOf) ps
  , LT.isVarArg = False
  }

genFunction :: Func Type TypedID -> GenDec ()
genFunction Func{ name, captures = Nothing, params, body } = do
  let funcName = fromString $ show $ pPrint name
  let llvmParams = map (\(ID _ _ ty) -> (convertType ty, NoParameterName)) params
  let retty = convertType (typeOf body)
  void $ function funcName llvmParams retty $ \args ->
    local (\st -> st { variableMap = fromList (zip params args) }) $
    genTermExpr body
genFunction Func{ name, captures = Just caps, mutrecs, params, body } = do
  let funcName = fromString $ show $ pPrint name
  let llvmParams = (LT.ptr LT.i8, NoParameterName) : map (\(ID _ _ ty) -> (convertType ty, NoParameterName)) params
  let retty = convertType (typeOf body)
  void $ function funcName llvmParams retty $ \(capsPtr : args) -> do
    capsMap <- genUnpackCaps capsPtr
    clsMap <- genCls capsPtr
    local (\st -> st { variableMap = fromList (zip params args) <> capsMap <> clsMap
                     , captures = capsPtr })
      (genTermExpr body)
  where
    genUnpackCaps :: O.Operand -> GenExpr (Map TypedID O.Operand)
    genUnpackCaps capsPtr = do
      capsPtr' <- bitcast capsPtr (LT.ptr $ LT.StructureType False (map (convertType . typeOf) caps))
      fmap mconcat $ forM (zip [0..] caps) $ \(i, c) -> do
        cPtr <- gep capsPtr' [int32 0, int32 i]
        cOpr <- load cPtr 0
        pure (Map.fromList [(c, cOpr)])
    genCls capsPtr = fmap mconcat $ forM mutrecs $ \f -> do
      clsPtr <- packClosure f capsPtr
      pure (Map.fromList [(f, clsPtr)])

packClosure :: (MonadReader GenState m, MonadIO m,
                MonadIRBuilder m, MonadModuleBuilder m) =>
  ID Type -> O.Operand -> m O.Operand
packClosure f capsPtr = do
  let LT.PointerType clsTy _ = convertType $ typeOf f
  clsPtr <- mallocType clsTy
  clsFunPtr <- gep clsPtr [int32 0, int32 0]
  funOpr <- getFun f
  store clsFunPtr 0 funOpr
  clsCapPtr <- gep clsPtr [int32 0, int32 1]
  store clsCapPtr 0 capsPtr
  pure clsPtr

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
  p <- global n (LT.ArrayType (toEnum $ length bytes + 1) LT.i8) (C.Array LT.i8 $ map (C.Int 8 . toInteger) (bytes <> [0]))
  bitcast p (LT.ptr LT.i8)
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
  byteSize <- mul sizeVal $ sizeof (convertType ty)
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
  pure $ undef $ LT.ptr $ LT.StructureType False []
genExpr (MakeClosure f cs) = do
  -- generate captures
  let capTy = LT.StructureType False (map (convertType . typeOf) cs)
  capPtr <- mallocType capTy
  forM_ (zip [0..] cs) $ \(i, c) -> do
    capElemPtr <- gep capPtr [int32 0, int32 i]
    valOpr <- getVar c
    store capElemPtr 0 valOpr

  -- generate closure
  packClosure f =<< bitcast capPtr (LT.ptr LT.i8)
genExpr (CallDirect f args) = do
  funOpr <- getFun f
  argOprs <- mapM (fmap (,[]) . getVar) args
  call funOpr argOprs
genExpr (CallWithCaptures f args) = do
  funOpr <- getFun f
  GenState { captures } <- ask
  argOprs <- ((captures, []) :) <$> mapM (fmap (,[]) . getVar) args
  call funOpr argOprs
genExpr (CallClosure f args) = do
  clsPtr <- getVar f
  clsFunPtr <- gep clsPtr [int32 0, int32 0]
  clsFunOpr <- load clsFunPtr 0
  clsCapPtr <- gep clsPtr [int32 0, int32 1]
  clsCapOpr <- load clsCapPtr 0
  argOprs <- ((clsCapOpr, []) :) <$> mapM (fmap (,[]) . getVar) args
  call clsFunOpr argOprs
genExpr (Let defs e) = do
  defsMap <- mapM (\(x, val) -> (x, ) <$> genExpr val) defs
  local (\st -> st { variableMap = fromList defsMap <> variableMap st }) $ genExpr e
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
genExpr (Prim orig ty xs) = do
  GenState { prims } <- ask
  psMap <- readIORef prims
  f <- case lookup orig psMap of
    Just f -> pure f -- call f =<< mapM (fmap (,[]) . getVar) xs
    Nothing -> do
      (argtys, retty) <- case ty of
        (TyFun ps r) -> pure (map convertType ps, convertType r)
        _            -> error "extern symbol must have a function type"
      f <- extern (fromString $ toString orig) argtys retty
      modifyIORef prims (Map.insert orig f)
      pure f
  args <- mapM (fmap (,[]) . getVar) xs
  call f args
genExpr (BinOp op x y) = do
  xOpr <- getVar x
  yOpr <- getVar y
  opInstr xOpr yOpr
  where
    opInstr = case (op, convertType $ typeOf x) of
      (Add, _) -> add; (Sub, _) -> sub; (Mul, _) -> mul; (Div, _) -> sdiv;
      (Mod, _) -> srem;
      (FAdd, _) -> fadd; (FSub, _) -> fsub; (FMul, _) -> fmul; (FDiv, _) -> fdiv;
      (Eq, LT.IntegerType _) -> icmp IP.EQ
      (Eq, LT.FloatingPointType _) -> fcmp FP.OEQ
      (Neq, LT.IntegerType _) -> icmp IP.NE
      (Neq, LT.FloatingPointType _) -> fcmp FP.ONE
      (Lt, LT.IntegerType _) -> icmp IP.SLT
      (Lt, LT.FloatingPointType _) -> fcmp FP.OLT
      (Gt, LT.IntegerType _) -> icmp IP.SGT
      (Gt, LT.FloatingPointType _) -> fcmp FP.OGT
      (Le, LT.IntegerType _) -> icmp IP.SLE
      (Le, LT.FloatingPointType _) -> fcmp FP.OLE
      (Ge, LT.IntegerType _) -> icmp IP.SGE
      (Ge, LT.FloatingPointType _) -> fcmp FP.OGE
      (And, _) -> IRBuilder.and
      (Or, _) -> IRBuilder.or
      (_, t) -> error $ show t <> " is not comparable"

-- wrapped IRBuilder and utilities

store :: (HasCallStack, MonadIRBuilder m) => O.Operand -> Word32 -> O.Operand -> m ()
store ptr align val =
  assert (isPointerType $ LT.typeOf ptr) $
  assert (LT.pointerReferent (LT.typeOf ptr) == LT.typeOf val) $
  IRBuilder.store ptr align val

sizeof :: LT.Type -> O.Operand
sizeof ty = O.ConstantOperand $ C.PtrToInt szPtr LT.i64
  where
    ptrType = LT.ptr ty
    nullPtr = C.IntToPtr (C.Int 32 0) ptrType
    szPtr = C.GetElementPtr True nullPtr [C.Int 32 1]

undef :: LT.Type -> O.Operand
undef ty = O.ConstantOperand $ C.Undef ty

isPointerType :: LT.Type -> Bool
isPointerType LT.PointerType{} = True
isPointerType _                = False

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.BackEnd.GenLLVM
  ( GenLLVM
  )
where

import qualified Data.Map.Strict               as Map
import           Language.Malgo.ID
import           Language.Malgo.IR.LIR         as IR
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.TypeRep.LType  as IR
import qualified LLVM.AST
import           LLVM.AST.Constant              ( Constant(..) )
import qualified LLVM.AST.Constant             as C
import qualified LLVM.AST.FloatingPointPredicate
                                               as FP
import qualified LLVM.AST.IntegerPredicate     as IP
import           LLVM.AST.Operand               ( Operand(..) )
import           LLVM.AST.Type           hiding ( double
                                                , void
                                                )
import qualified LLVM.AST.Type                 as LT
import           LLVM.IRBuilder
import qualified LLVM.IRBuilder                as IRBuilder

data GenLLVM

instance Pass GenLLVM (IR.Program (ID LType)) [LLVM.AST.Definition] where
  passName = "GenLLVM"
  isDump _ = False
  trans Program { functions, mainFunc } = dumpLLVM $ do
    let funMap = foldMap genFunMap functions
    local (\st -> st { variableMap = funMap }) $ do
      mapM_ genFunction functions
      void $ function "main" [] LT.i32 $ \_ -> genBlock mainFunc (const (ret (int32 0)))
   where
    genFunMap :: Func (ID LType) -> Map (ID LType) Operand
    genFunMap Func { name } =
      let Function r ps = ltypeOf name
      in  one
            ( name
            , ConstantOperand
            $ GlobalReference
                (ptr $ FunctionType { resultType    = convertType r
                                    , argumentTypes = map convertType ps
                                    , isVarArg      = False
                                    }
                )
            $ genFuncName name
            )

data GenState = GenState { variableMap :: Map (ID LType) Operand
                         , prims       :: IORef (Map Text Operand)
                         }

type GenExpr a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (ReaderT GenState MalgoM)

dumpLLVM :: MonadIO m => ModuleBuilderT (ReaderT GenState m) a -> m [LLVM.AST.Definition]
dumpLLVM m = do
  p <- newIORef mempty
  let genState = GenState { variableMap = mempty, prims = p }
  usingReaderT genState $ execModuleBuilderT emptyModuleBuilder m

convertType :: LType -> Type
convertType (Ptr x)         = ptr (convertType x)
convertType Bit             = i1
convertType I32             = i32
convertType I64             = i64
convertType U8              = i8
convertType U32             = i32
convertType U64             = i64
convertType F64             = LT.double
convertType SizeT           = i64
convertType (IR.Struct xs ) = StructureType False (map convertType xs)
convertType (Function r ps) = ptr $ FunctionType (convertType r) (map convertType ps) False
convertType Void            = LT.void

findVar :: MonadReader GenState m => ID LType -> m Operand
findVar i = do
  m <- asks variableMap
  pure $ lookupDefault (error $ show i <> " is not found in " <> show m) i m

findExt :: (MonadReader GenState m, MonadIO m, MonadModuleBuilder m)
        => Text
        -> [Type]
        -> Type
        -> m Operand
findExt name ps r = do
  GenState { prims } <- ask
  psMap              <- readIORef prims
  case lookup name psMap of
    Just opr -> pure opr
    Nothing  -> do
      opr <- extern (LLVM.AST.mkName $ toString name) ps r
      modifyIORef prims (Map.insert name opr)
      pure opr

mallocBytes :: Operand -> Maybe Type -> GenExpr Operand
mallocBytes bytesOpr maybeType = do
  gcMalloc <- findExt "GC_malloc" [i64] (ptr i8)
  ptrOpr   <- call gcMalloc [(bytesOpr, [])]
  case maybeType of
    Just t  -> bitcast ptrOpr (ptr t)
    Nothing -> pure ptrOpr

genFuncName :: ID a -> LLVM.AST.Name
genFuncName ID { idName, idUniq } = LLVM.AST.mkName $ toString $ idName <> show idUniq

genFunction :: Func (ID LType) -> GenDec ()
genFunction Func { name, params, body } = void $ function funcName llvmParams retty $ \args ->
  local (\st -> st { variableMap = fromList (zip params args) <> variableMap st })
    $ genBlock body ret
 where
  funcName   = genFuncName name
  llvmParams = map (\ID { idMeta } -> (convertType idMeta, NoParameterName)) params
  retty      = convertType (ltypeOf body)

genBlock :: Block (ID LType) -> (Operand -> GenExpr a) -> GenExpr a
genBlock Block { insts, value } term = go insts
 where
  go []               = term =<< findVar value
  go ((x, inst) : xs) = do
    opr <- genInst inst
    local (\st -> st { variableMap = insert x opr (variableMap st) }) $ go xs

genInst :: HasCallStack => Inst (ID LType) -> GenExpr Operand
genInst (Constant x) = genConstant x
genInst (Call f xs ) = do
  f'  <- findVar f
  xs' <- mapM (fmap (, []) . findVar) xs
  call f' xs'
genInst (CallExt f (Function r ps) xs) = do
  f'  <- findExt f (map convertType ps) (convertType r)
  xs' <- mapM (fmap (, []) . findVar) xs
  call f' xs'
genInst CallExt{}             = error "extern symbol must have a function type"
genInst (ArrayCreate ty size) = do
  let ty' = convertType ty
  size' <- mul (sizeof ty') =<< findVar size
  raw   <- mallocBytes size' (Just ty')
  arr   <- mallocBytes (sizeof $ StructureType False [LT.ptr ty', i64])
                       (Just $ StructureType False [LT.ptr ty', i64])
  arrRawAddr <- gep arr [int32 0, int32 0]
  store arrRawAddr 0 raw
  arrSizeAddr <- gep arr [int32 0, int32 1]
  store arrSizeAddr 0 =<< findVar size
  pure arr
genInst (Alloca ty) = do
  let size = sizeof (convertType ty)
  mallocBytes size (Just $ convertType ty)
genInst (LoadC x is) = do
  xOpr    <- findVar x
  valAddr <- gep xOpr (map (int32 . toInteger) is)
  load valAddr 0
genInst (Load x i) = do
  xOpr    <- findVar x
  iOpr    <- findVar i
  valAddr <- gep xOpr [iOpr]
  load valAddr 0
genInst (StoreC x is val) = do
  xOpr    <- findVar x
  valOpr  <- findVar val
  valAddr <- gep xOpr (map (int32 . toInteger) is)
  store valAddr 0 valOpr
  pure $ ConstantOperand $ C.Undef LT.VoidType
genInst (Store x is val) = do
  xOpr    <- findVar x
  iOprs   <- mapM findVar is
  valOpr  <- findVar val
  valAddr <- gep xOpr iOprs
  store valAddr 0 valOpr
  pure $ ConstantOperand $ C.Undef LT.VoidType
genInst (Cast ty x) = do
  xOpr <- findVar x
  case (ty, ltypeOf x) of
    (Ptr ty1, Ptr _) -> bitcast xOpr (ptr $ convertType ty1)
    (Ptr ty1, I64  ) -> inttoptr xOpr (ptr $ convertType ty1)
    (Ptr ty1, U64  ) -> inttoptr xOpr (ptr $ convertType ty1)
    (Ptr ty1, SizeT) -> inttoptr xOpr (ptr $ convertType ty1)
    (Ptr ty1, F64  ) -> do
      p <- alloca LT.double Nothing 0
      store p 0 xOpr
      p' <- bitcast p (ptr i64)
      i  <- load p' 0
      inttoptr i (ptr $ convertType ty1)
    (I64  , Ptr{}) -> ptrtoint xOpr i64
    (U64  , Ptr{}) -> ptrtoint xOpr i64
    (SizeT, Ptr{}) -> ptrtoint xOpr i64
    (F64  , Ptr{}) -> do
      i <- ptrtoint xOpr i64
      p <- alloca i64 Nothing 0
      store p 0 i
      p' <- bitcast p (ptr LT.double)
      load p' 0
    (I64  , SizeT) -> pure xOpr
    (U64  , SizeT) -> pure xOpr
    (SizeT, I64  ) -> pure xOpr
    (SizeT, U64  ) -> pure xOpr
    _              -> error "invalid cast"
genInst (IR.Trunc ty x) = do
  xOpr <- findVar x
  trunc xOpr (convertType ty)
genInst (Zext ty x) = do
  xOpr <- findVar x
  zext xOpr (convertType ty)
genInst (Sext ty x) = do
  xOpr <- findVar x
  sext xOpr (convertType ty)
genInst (IR.Undef ty ) = pure $ ConstantOperand $ C.Undef (convertType ty)
genInst (BinOp op x y) = do
  xOpr <- findVar x
  yOpr <- findVar y
  genBinOp op xOpr yOpr
genInst (If cond thenBlock elseBlock) = do
  cOpr      <- findVar cond
  result    <- alloca (convertType (ltypeOf thenBlock)) Nothing 0
  thenLabel <- freshName "then"
  elseLabel <- freshName "else"
  endLabel  <- freshName "end"
  condBr cOpr thenLabel elseLabel
  emitBlockStart thenLabel
  genBlock thenBlock (\o -> store result 0 o >> br endLabel)
  emitBlockStart elseLabel
  genBlock elseBlock (\o -> store result 0 o >> br endLabel)
  emitBlockStart endLabel
  load result 0
genInst (For index from to body) = do
  condLabel <- freshName "cond"
  bodyLabel <- freshName "body"
  endLabel  <- freshName "end"
  -- for (i64 i = from;
  iPtr      <- alloca i64 Nothing 0
  store iPtr 0 =<< findVar from
  br condLabel

  -- cond: i < to;
  emitBlockStart condLabel
  iOpr <- load iPtr 0
  cond <- icmp IP.SLT iOpr =<< findVar to
  condBr cond bodyLabel endLabel

  -- body: genBlock body
  emitBlockStart bodyLabel
  local (\st -> st { variableMap = insert index iOpr $ variableMap st }) $ genBlock body $ \_ -> do
    -- i++)
    store iPtr 0 =<< add iOpr (int64 1)
    br condLabel

  -- end: }
  emitBlockStart endLabel
  pure $ ConstantOperand $ C.Undef LT.VoidType

genConstant :: IR.Constant -> GenExpr Operand
genConstant (Bool    True ) = pure $ bit 1
genConstant (Bool    False) = pure $ bit 0
genConstant (Int32   x    ) = pure $ int32 $ toInteger x
genConstant (Int64   x    ) = pure $ int64 $ toInteger x
genConstant (Word8   x    ) = pure $ int8 $ toInteger x
genConstant (Word32  x    ) = pure $ int32 $ toInteger x
genConstant (Word64  x    ) = pure $ int64 $ toInteger x
genConstant (Float64 x    ) = pure $ double x
genConstant (String  bytes) = do
  n <- fresh
  p <- global n
              (ArrayType (fromIntegral $ length bytes + 1) i8)
              (Array i8 $ map (Int 8 . toInteger) (bytes <> [0]))
  bitcast p (ptr i8)

genBinOp :: MonadIRBuilder m => IR.Op -> Operand -> Operand -> m Operand
genBinOp ADD  = add
genBinOp SUB  = sub
genBinOp MUL  = mul
genBinOp SDIV = sdiv
genBinOp SREM = srem
genBinOp UDIV = udiv
genBinOp UREM = urem
genBinOp FADD = fadd
genBinOp FSUB = fsub
genBinOp FMUL = fmul
genBinOp FDIV = fdiv
genBinOp IEQ  = icmp IP.EQ
genBinOp INE  = icmp IP.NE
genBinOp SLT  = icmp IP.SLT
genBinOp SGT  = icmp IP.SGT
genBinOp SLE  = icmp IP.SLE
genBinOp SGE  = icmp IP.SGE
genBinOp ULT  = icmp IP.ULT
genBinOp UGT  = icmp IP.UGT
genBinOp ULE  = icmp IP.ULE
genBinOp UGE  = icmp IP.UGE
genBinOp FEQ  = fcmp FP.OEQ
genBinOp FNE  = fcmp FP.ONE
genBinOp FLT  = fcmp FP.OLT
genBinOp FGT  = fcmp FP.OGT
genBinOp FLE  = fcmp FP.OLE
genBinOp FGE  = fcmp FP.OGE
genBinOp AND  = IRBuilder.and
genBinOp OR   = IRBuilder.or

sizeof :: Type -> Operand
sizeof ty = ConstantOperand $ C.PtrToInt szPtr LT.i64
 where
  ptrType = LT.ptr ty
  nullPtr = C.IntToPtr (C.Int 32 0) ptrType
  szPtr   = C.GetElementPtr True nullPtr [C.Int 32 1]

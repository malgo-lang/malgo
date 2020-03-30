{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Language.Malgo.BackEnd.GenLLVM
  ( GenLLVM
  )
where

import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.IR.LIR         as IR

import           Language.Malgo.TypeRep.LType  as IR

import           LLVM.AST.Constant              ( Constant(..) )
import           LLVM.AST.Operand               ( Operand(..) )
import           LLVM.AST.Type           hiding ( double
                                                , void
                                                )
import           LLVM.IRBuilder
import qualified LLVM.AST
import qualified LLVM.AST.Constant             as C
import qualified LLVM.AST.FloatingPointPredicate
                                               as FP
import qualified LLVM.AST.IntegerPredicate     as IP
import qualified LLVM.AST.Type                 as LT
import qualified LLVM.IRBuilder                as IRBuilder

import qualified Data.Map                      as Map

data GenLLVM

instance Pass GenLLVM (IR.Program (Id LType)) [LLVM.AST.Definition] where
  passName = "GenLLVM"
  isDump _ = False
  trans Program { functions, mainFunc } =
    dumpLLVM
      $ local
          (const $ foldMap
            (\Func { name } -> Map.singleton
              name
              (ConstantOperand $ GlobalReference (convertType (ltypeOf name)) $ genName name)
            )
            functions
          )
      $ do
          mapM_ genFunction functions
          void $ function "main" [] LT.i32 $ \_ -> genBlock mainFunc (const (ret (int32 0)))

type GenExpr a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (ReaderT OprMap (StateT PrimMap MalgoM))
type OprMap = Map (Id LType) Operand
type PrimMap = Map String Operand

-- dumpLLVM :: MonadIO m => ModuleBuilderT (ReaderT GenState m) a -> m [LLVM.AST.Definition]
dumpLLVM :: GenDec a -> MalgoM [LLVM.AST.Definition]
dumpLLVM m =
  evalStateT ?? mempty $ runReaderT ?? mempty $ execModuleBuilderT emptyModuleBuilder m

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

findVar :: MonadReader OprMap m => Id LType -> m Operand
findVar i = fromMaybe (error $ show $ pPrint i <> " is not found") . view (at i) <$> ask

findExt :: (MonadState PrimMap m, MonadModuleBuilder m) => String -> [Type] -> Type -> m Operand
findExt name ps r = do
  psMap <- get
  case view (at name) psMap of
    Just opr -> pure opr
    Nothing  -> do
      opr <- extern (LLVM.AST.mkName name) ps r
      modify (set (at name) $ Just opr)
      pure opr

mallocBytes :: Operand -> Maybe Type -> GenExpr Operand
mallocBytes bytesOpr maybeType = do
  gcMalloc <- findExt "GC_malloc" [i64] (ptr i8)
  ptrOpr   <- call gcMalloc [(bytesOpr, [])]
  case maybeType of
    Just t  -> bitcast ptrOpr (ptr t)
    Nothing -> pure ptrOpr

genName :: Id a -> LLVM.AST.Name
genName Id { idName, idUniq } = LLVM.AST.mkName $ idName <> show idUniq

genFunction :: Func (Id LType) -> GenDec ()
genFunction Func { name, params, body } = void $ function funcName llvmParams retty $ \args ->
  local (Map.fromList (zip params args) <>) $ genBlock body ret
 where
  funcName   = genName name
  llvmParams = map (\Id { idMeta } -> (convertType idMeta, NoParameterName)) params
  retty      = convertType (ltypeOf body)

genBlock :: Block (Id LType) -> (Operand -> GenExpr a) -> GenExpr a
genBlock Block { insns, value } term = do
  env' <- (foldlM (\e i -> local (e <>) (genInsn i <*> ask)) ?? insns) =<< ask
  local (env' <>) $ term =<< findVar value
 where
  genInsn (Assign x e) = do
    opr <- genExpr e
    pure (at x ?~ opr)
  genInsn (StoreC x is val) = do
    xOpr   <- findVar x
    valOpr <- findVar val
    xAddr  <- gep xOpr (map (int32 . toInteger) is)
    store xAddr 0 valOpr
    pure id
  genInsn (Store x is val) = do
    xOpr   <- findVar x
    iOprs  <- mapM findVar is
    valOpr <- findVar val
    xAddr  <- gep xOpr iOprs
    store xAddr 0 valOpr
    pure id
  genInsn (For index from to body) = mdo
    -- for (i64 i = from;
    iPtr <- alloca i64 Nothing 0
    store iPtr 0 =<< findVar from
    br condLabel

    -- cond: i < to;
    condLabel <- block `named` "cond"
    iOpr      <- load iPtr 0
    cond      <- icmp IP.SLT iOpr =<< findVar to
    condBr cond bodyLabel endLabel

    -- body: genBlock body
    bodyLabel <- block `named` "body"
    local (set (at index) $ Just iOpr) $ genBlock body $ \_ -> do
      -- i++)
      store iPtr 0 =<< add iOpr (int64 1)
      br condLabel

    -- end: }
    endLabel <- block `named` "end"
    pure id

genExpr :: Expr (Id LType) -> GenExpr Operand
genExpr (Constant x) = genConstant x
genExpr (Call f xs ) = do
  f'  <- findVar f
  xs' <- mapM (fmap (, []) . findVar) xs
  call f' xs'
genExpr (CallExt f (Function r ps) xs) = do
  f'  <- findExt f (map convertType ps) (convertType r)
  xs' <- mapM (fmap (, []) . findVar) xs
  call f' xs'
genExpr CallExt{}          = error "extern symbol must have a function type"
genExpr (ArrayCreate ty n) = do
  let ty' = convertType ty
  size' <- mul (sizeof ty') =<< findVar n
  raw   <- mallocBytes size' (Just ty')
  arr   <- mallocBytes (sizeof $ StructureType False [LT.ptr ty', i64])
                       (Just $ StructureType False [LT.ptr ty', i64])
  arrRawAddr <- gep arr [int32 0, int32 0]
  store arrRawAddr 0 raw
  arrSizeAddr <- gep arr [int32 0, int32 1]
  store arrSizeAddr 0 =<< findVar n
  pure arr
genExpr (Alloca ty ) = mallocBytes (sizeof (convertType ty)) (Just $ convertType ty)
genExpr (LoadC x is) = do
  xOpr    <- findVar x
  valAddr <- gep xOpr (map (int32 . toInteger) is)
  load valAddr 0
genExpr (Load _ x is) = do
  xOpr    <- findVar x
  iOprs   <- mapM findVar is
  valAddr <- gep xOpr iOprs
  load valAddr 0
genExpr (Cast ty x) = do
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
genExpr (IR.Trunc ty x) = do
  xOpr <- findVar x
  trunc xOpr (convertType ty)
genExpr (Zext ty x) = do
  xOpr <- findVar x
  zext xOpr (convertType ty)
genExpr (Sext ty x) = do
  xOpr <- findVar x
  sext xOpr (convertType ty)
genExpr (IR.Undef ty ) = pure $ ConstantOperand $ C.Undef (convertType ty)
genExpr (BinOp op x y) = do
  xOpr <- findVar x
  yOpr <- findVar y
  genBinOp op xOpr yOpr
genExpr (If cond thenBlock elseBlock) = mdo
  cOpr   <- findVar cond
  result <- alloca (convertType (ltypeOf thenBlock)) Nothing 0
  condBr cOpr thenLabel elseLabel
  thenLabel <- block `named` "then"
  genBlock thenBlock (\o -> store result 0 o >> br endLabel)
  elseLabel <- block `named` "else"
  genBlock elseBlock (\o -> store result 0 o >> br endLabel)
  endLabel <- block `named` "end"
  load result 0

genConstant :: IR.Constant -> GenExpr Operand
genConstant (Bool    True ) = pure $ bit 1
genConstant (Bool    False) = pure $ bit 0
genConstant (Int32   x    ) = pure $ int32 $ toInteger x
genConstant (Int64   x    ) = pure $ int64 $ toInteger x
genConstant (Word8   x    ) = pure $ int8 $ toInteger x
genConstant (Word32  x    ) = pure $ int32 $ toInteger x
genConstant (Word64  x    ) = pure $ int64 $ toInteger x
genConstant (Float64 x    ) = pure $ double x
genConstant (String  str  ) = do
  i <- lift $ lift getUniq
  let n = fromString ("$globle_str_" <> show i)
  ConstantOperand <$> globalStringPtr str n

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

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | LLVM Code Generator
module Koriel.Core.CodeGen.LLVM (
  codeGen,
)
where

import Control.Lens (At (at), ifor, ifor_, makeFieldsNoPrefix, over, use, view, (<?=), (?=), (?~))
import Control.Monad.Cont (ContT (..), withContT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.State.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.Extra (headDef, maximum, mconcatMap)
import Data.String.Conversions
import Data.Traversable (for)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Koriel.Core.Op qualified as Op
import Koriel.Core.Syntax
import Koriel.Core.Type hiding (typeOf)
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import LLVM.AST (
  Definition (..),
  Module (..),
  Name,
  defaultModule,
  mkName,
 )
import LLVM.AST.Constant qualified as C
import LLVM.AST.FloatingPointPredicate qualified as FP
import LLVM.AST.Global
import LLVM.AST.IntegerPredicate qualified as IP
import LLVM.AST.Linkage (Linkage (External, Internal))
import LLVM.AST.Operand (Operand (..))
import LLVM.AST.Type hiding (
  double,
  void,
 )
import LLVM.AST.Type qualified as LT
import LLVM.AST.Typed (typeOf)
import LLVM.Context (withContext)
import LLVM.IRBuilder hiding (globalStringPtr, sizeof)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import Relude.Unsafe qualified as Unsafe

-- | 'PrimMap' is a map from primitive function name to its LLVM function.
type PrimMap = Map Name Operand

-- 変数のHashMapとknown関数のHashMapを分割する
-- #7(https://github.com/takoeight0821/malgo/issues/7)のようなバグの早期検出が期待できる
data CodeGenEnv = CodeGenEnv
  { uniqSupply :: UniqSupply,
    -- In optimization, some variables are defined multiple times.
    -- So, we need to treat them as as scoped variables.
    _valueMap :: HashMap (Id C.Type) Operand,
    _globalValueMap :: HashMap (Id C.Type) Operand,
    _funcMap :: HashMap (Id C.Type) Operand,
    moduleName :: ModuleName,
    isAllocaResult :: Bool
  }

makeFieldsNoPrefix ''CodeGenEnv

newCodeGenEnv :: UniqSupply -> ModuleName -> Program (Id C.Type) -> CodeGenEnv
newCodeGenEnv uniqSupply moduleName Program {..} =
  CodeGenEnv
    { uniqSupply = uniqSupply,
      _valueMap = mempty,
      _globalValueMap = varMap,
      _funcMap = funcMap,
      moduleName = moduleName,
      isAllocaResult = False
    }
  where
    -- topVarsのOprMapを作成
    varMap = mconcatMap ?? topVars $ \(v, _, _) ->
      one (v, ConstantOperand $ C.GlobalReference $ toName v)
    -- topFuncsのOprMapを作成
    funcMap = mconcatMap ?? topFuns $ \(f, _, _, _) ->
      one (f, ConstantOperand $ C.GlobalReference $ toName f)

type MonadCodeGen m =
  ( MonadModuleBuilder m,
    MonadReader CodeGenEnv m,
    MonadState PrimMap m
  ) ::
    Constraint

runCodeGenT :: Monad m => CodeGenEnv -> Lazy.StateT PrimMap (ReaderT CodeGenEnv (ModuleBuilderT m)) a -> m [Definition]
runCodeGenT env m =
  execModuleBuilderT emptyModuleBuilder $
    runReaderT (Lazy.evalStateT m mempty) env

-- | Generate LLVM IR from a program.
codeGen ::
  (MonadFix m, MonadFail m, MonadIO m) =>
  -- | Source file path
  FilePath ->
  -- | Destination file path
  FilePath ->
  -- | Unique supply
  UniqSupply ->
  -- | Module name of the source program
  ModuleName ->
  -- | Entry point of the source program
  Maybe (Id C.Type) ->
  -- | Source program
  Program (Id C.Type) ->
  m ()
codeGen srcPath dstPath uniqSupply modName mentry Program {..} = do
  llvmir <- runCodeGenT (newCodeGenEnv uniqSupply modName Program {..}) do
    _ <- typedef (mkName "struct.bucket") Nothing -- (Just $ StructureType False [ptr i8, ptr i8, ptr $ NamedTypeReference (mkName "struct.bucket")])
    _ <- typedef (mkName "struct.hash_table") Nothing -- (Just $ StructureType False [ArrayType 16 (NamedTypeReference (mkName "struct.bucket")), i64])
    void $ extern "GC_init" [] LT.void
    for_ extFuns \(name, typ) -> do
      let name' = LLVM.AST.mkName $ convertString name
      case typ of
        ps :-> r -> extern name' (map convType ps) (convType r)
        _ -> error "invalid type"
    traverse_ (\(n, _, e) -> genVar n e) topVars
    traverse_ (\(f, ps, _, body) -> genFunc f ps body) topFuns
    case mentry of
      Just entry -> do
        (f, (ps, body)) <-
          mainFunc =<< runDef do
            let unitCon = C.Con C.Tuple []
            unit <- let_ (SumT [unitCon]) (Pack (SumT [unitCon]) unitCon [])
            _ <- bind $ CallDirect entry [unit]
            pure (Atom $ Unboxed $ Int32 0)
        void $ genFunc f ps body
      Nothing -> pass
    genLoadModule $ runContT (initTopVars topVars) (\_ -> retVoid)
  let llvmModule =
        defaultModule
          { LLVM.AST.moduleName = fromString srcPath,
            moduleSourceFileName = fromString srcPath,
            moduleDefinitions = llvmir
          }
  liftIO $ withContext $ \ctx -> writeFileBS dstPath =<< withModuleFromAST ctx llvmModule moduleLLVMAssembly
  where
    initTopVars [] = pure ()
    initTopVars ((name, _, expr) : xs) = do
      view (globalValueMap . at name) >>= \case
        Nothing -> error $ show $ pPrint name <+> "is not found"
        Just name' -> do
          eOpr <- genExpr expr
          store name' 0 eOpr
          initTopVars xs
    -- Generate main function.
    mainFunc e = do
      -- `Builtin.main` are compiled as `main` in `Koriel.Core.CodeGen.toName`
      mainFuncId <- newNativeId "main" ([] :-> Int32T)
      mainFuncBody <- runDef do
        _ <- bind $ RawCall "GC_init" ([] :-> VoidT) []
        _ <- bind $ RawCall ("koriel_load_" <> modName.raw) ([] :-> VoidT) []
        pure e
      pure (mainFuncId, ([], mainFuncBody))

convType :: C.Type -> LT.Type
convType (_ :-> _) = ptr
convType Int32T = i32
convType Int64T = i64
convType FloatT = LT.float
convType DoubleT = LT.double
convType CharT = i8
convType StringT = ptr
convType BoolT = i8
convType (SumT _) = ptr
convType (PtrT _) = ptr
convType (RecordT _) = ptr
convType AnyT = ptr
convType VoidT = LT.void

innerType :: C.Type -> LT.Type
innerType (_ :-> _) = StructureType False [ptr, ptr]
innerType StringT = i8
innerType (SumT cs) =
  let size = maximum $ sizeofCon <$> toList cs
   in StructureType False [i8, if size == 0 then StructureType False [] else LT.VectorType size i8]
innerType (PtrT ty) = convType ty
innerType (RecordT _) = LT.NamedTypeReference (mkName "struct.hash_table")
innerType AnyT = i8
innerType _ = error "invalid type"

sizeofCon :: Num a => Con -> a
sizeofCon (Con _ ts) = sum $ map sizeofType ts

sizeofType :: Num a => C.Type -> a
sizeofType (_ :-> _) = 8
sizeofType Int32T = 4
sizeofType Int64T = 8
sizeofType FloatT = 4
sizeofType DoubleT = 8
sizeofType CharT = 1
sizeofType StringT = 8
sizeofType BoolT = 1
sizeofType (SumT _) = 8
sizeofType (PtrT _) = 8
sizeofType (RecordT _) = 8
sizeofType AnyT = 8
sizeofType VoidT = 0

-- | Change the current terminator.
withTerminator ::
  -- | Action
  ContT () m Operand ->
  -- | Builder of the new terminator based on the current terminator
  ( (Operand -> m ()) ->
    -- \^ Current terminator
    Operand ->
    m ()
  ) ->
  ContT () m Operand
withTerminator = flip withContT

findVar :: (MonadCodeGen m, MonadIRBuilder m) => Id C.Type -> m Operand
findVar x = findLocalVar
  where
    findLocalVar =
      view (valueMap . at x) >>= \case
        Just opr -> pure opr
        Nothing -> findGlobalVar
    findGlobalVar =
      view (globalValueMap . at x) >>= \case
        Just opr -> load (convType $ C.typeOf x) opr 0 -- global variable is a pointer to the actual value
        Nothing -> findExtVar
    findExtVar =
      use (at $ toName x) >>= \case
        Just opr -> load (convType $ C.typeOf x) opr 0
        Nothing -> internExtVar
    internExtVar = do
      emitDefn $
        GlobalDefinition
          globalVariableDefaults
            { LLVM.AST.Global.name = toName x,
              LLVM.AST.Global.type' = convType $ C.typeOf x,
              linkage = LLVM.AST.Linkage.External
            }
      let opr = ConstantOperand (C.GlobalReference (toName x))
      at (toName x) ?= opr
      load (convType $ C.typeOf x) opr 0

findFun :: MonadCodeGen m => Id C.Type -> m Operand
findFun x =
  view (funcMap . at x) >>= \case
    Just opr -> pure opr
    Nothing ->
      case C.typeOf x of
        ps :-> r -> findExt (toName x) (map convType ps) (convType r)
        _ -> error $ show $ pPrint x <> " is not found"

-- まだ生成していない外部関数を呼び出そうとしたら、externする
-- すでにexternしている場合は、そのOperandを返す
findExt :: MonadCodeGen m => Name -> [LT.Type] -> LT.Type -> m Operand
findExt x ps r =
  use (at x) >>= \case
    Just x -> pure x
    Nothing -> (at x <?=) =<< extern x ps r

mallocBytes ::
  (MonadCodeGen m, MonadIRBuilder m) =>
  Operand ->
  Maybe LT.Type ->
  m Operand
mallocBytes bytesOpr _maybeType = do
  gcMalloc <- findExt "malgo_malloc" [i64] ptr
  call (FunctionType ptr [i64] False) gcMalloc [(bytesOpr, [])]

mallocType :: (MonadCodeGen m, MonadIRBuilder m) => LT.Type -> m Operand
mallocType ty = mallocBytes (ConstantOperand $ sizeof ty) (Just ptr)

sizeof :: LT.Type -> C.Constant
sizeof ty = C.PtrToInt szPtr LT.i64
  where
    ptrType = LT.ptr
    nullPtr = C.IntToPtr (C.Int 32 0) ptrType
    szPtr = C.GetElementPtr True ty nullPtr [C.Int 32 1]

toName :: Id a -> LLVM.AST.Name
toName id = LLVM.AST.mkName $ convertString $ idToText id

-- generate code for a toplevel variable definition
genVar :: MonadModuleBuilder m => Id C.Type -> Expr (Id C.Type) -> m Operand
genVar name expr = global (toName name) (convType $ C.typeOf expr) (C.Undef (convType $ C.typeOf expr))

genLoadModule :: (MonadModuleBuilder m, MonadReader CodeGenEnv m) => IRBuilderT m () -> m Operand
genLoadModule m = do
  ModuleName modName <- asks (.moduleName)
  function (LLVM.AST.mkName $ convertString $ "koriel_load_" <> modName) [] LT.void $ const m

-- generate code for a 'known' function
genFunc ::
  ( MonadCodeGen m,
    MonadFix m,
    MonadFail m,
    MonadIO m
  ) =>
  Id C.Type ->
  [Id C.Type] ->
  Expr (Id C.Type) ->
  m Operand
genFunc name params body = do
  moduleName <- asks (.moduleName)
  let funcBuilder =
        if (idIsExternal name || idIsNative name) && name.moduleName == moduleName
          then function
          else internalFunction
  funcBuilder funcName llvmParams retty $ \args ->
    local (over valueMap (HashMap.fromList (zip params args) <>)) $ runContT (genExpr body) ret
  where
    funcName = toName name
    llvmParams =
      map
        (\x -> (convType $ x.meta, ParameterName $ toShort $ encodeUtf8 $ idToText x))
        params
    retty = convType (C.typeOf body)

genExpr ::
  ( MonadIRBuilder m,
    MonadCodeGen m,
    MonadFail m,
    MonadFix m,
    MonadIO m
  ) =>
  Expr (Id C.Type) ->
  ContT () m Operand
genExpr (Atom x) = genAtom x
genExpr e@(Call f xs) = do
  fOpr <- genAtom f
  captureAddr <- gep (innerType $ C.typeOf f) fOpr [int32 0, int32 0]
  captureOpr <- load ptr captureAddr 0
  funcAddr <- gep (innerType $ C.typeOf f) fOpr [int32 0, int32 1]
  funcOpr <- load ptr funcAddr 0
  xsOprs <- traverse genAtom xs
  call (FunctionType (convType $ C.typeOf e) (map (convType . C.typeOf) xs) False) funcOpr (map (,[]) $ captureOpr : xsOprs)
genExpr e@(CallDirect f xs) = do
  fOpr <- findFun f
  xsOprs <- traverse genAtom xs
  call (FunctionType (convType $ C.typeOf e) (map (convType . C.typeOf) xs) False) fOpr (map (,[]) xsOprs)
genExpr e@(RawCall name _ xs) = do
  let primOpr =
        ConstantOperand $
          C.GlobalReference $
            LLVM.AST.mkName $
              convertString name
  xsOprs <- traverse genAtom xs
  call (FunctionType (convType $ C.typeOf e) (map (convType . C.typeOf) xs) False) primOpr (map (,[]) xsOprs)
genExpr (BinOp o x y) = join (genOp o <$> genAtom x <*> genAtom y)
  where
    genOp Op.Add = add
    genOp Op.Sub = sub
    genOp Op.Mul = mul
    genOp Op.Div = sdiv
    genOp Op.Mod = srem
    genOp Op.FAdd = fadd
    genOp Op.FSub = fsub
    genOp Op.FMul = fmul
    genOp Op.FDiv = fdiv
    genOp Op.Eq = genCmpOp IP.EQ IP.EQ FP.OEQ
    genOp Op.Neq = genCmpOp IP.NE IP.NE FP.ONE
    genOp Op.Lt = genCmpOp IP.SLT IP.ULT FP.OLT
    genOp Op.Le = genCmpOp IP.SLE IP.ULE FP.OLE
    genOp Op.Gt = genCmpOp IP.SGT IP.UGT FP.OGT
    genOp Op.Ge = genCmpOp IP.SGE IP.UGE FP.OGE
    genOp Op.And = LLVM.IRBuilder.and
    genOp Op.Or = LLVM.IRBuilder.or
    genCmpOp signed unsigned ordered = \x' y' ->
      i1ToBool =<< case C.typeOf x of
        Int32T -> icmp signed x' y'
        Int64T -> icmp signed x' y'
        FloatT -> fcmp ordered x' y'
        DoubleT -> fcmp ordered x' y'
        CharT -> icmp unsigned x' y'
        StringT -> icmp unsigned x' y'
        BoolT -> icmp unsigned x' y'
        SumT _ -> icmp unsigned x' y'
        t -> error $ show t <> " is not comparable"
    i1ToBool i1opr = zext i1opr i8
genExpr (Cast ty x) = do
  xOpr <- genAtom x
  xOprTy <- typeOf xOpr
  if Right (convType ty) /= xOprTy
    then bitcast xOpr (convType ty)
    else pure xOpr
genExpr (Let xs e) = do
  env <- foldMapM prepare xs
  env <- lift $ local (over valueMap (env <>)) $ mconcat <$> traverse genLocalDef xs
  local (over valueMap (env <>)) $ genExpr e
  where
    -- Generate a `malloc(sizeof(<closure type>))` call for a local function definition.
    prepare (LocalDef name _ (Fun _ _)) =
      one . (name,)
        <$> mallocType (StructureType False [ptr, ptr])
    prepare _ = pure mempty
-- These `match` cases are not necessary.
genExpr (Match e (Bind _ _ body : _)) | C.typeOf e == VoidT = do
  _ <- genExpr e
  genExpr body
genExpr (Match e (Bind x _ body : _)) = do
  eOpr <- genExpr e
  local (over valueMap (at x ?~ eOpr)) (genExpr body)
genExpr (Match e cs)
  | C.typeOf e == VoidT = error "VoidT is not able to bind to variable."
  | otherwise = do
      isAllocaResult <- asks (.isAllocaResult)
      if isAllocaResult
        then do
          result <- alloca (convType $ C.typeOf (Match e cs)) Nothing 0
          eOpr <- genExpr e
          lift $ mdo
            let cont = \opr -> do
                  store result 0 opr
                  br resultBlock
            br switchBlock -- We need to end the current block before executing genCase
            -- 各ケースのコードとラベルを生成する
            -- switch用のタグがある場合は Right (タグ, ラベル) を、ない場合は Left タグ を返す
            (defaults, labels) <- partitionEithers <$> traverse (genCase eOpr (constructorList e) cont) cs
            -- defaultsの先頭を取り出し、switchのデフォルトケースとする
            -- defaultsが空の場合、デフォルトケースはunreachableにジャンプする
            defaultLabel <- headDef (withBlock "match_default" unreachable) $ map pure defaults
            switchBlock <- withBlock "match_switch" do
              tagOpr <- case C.typeOf e of
                SumT _ -> do
                  tagAddr <- gep (innerType $ C.typeOf e) eOpr [int32 0, int32 0]
                  load i8 tagAddr 0
                RecordT _ -> pure $ int32 0 -- Tag value must be integer, so we use 0 as default value.
                _ -> pure eOpr
              switch tagOpr defaultLabel labels
            resultBlock <- block `named` "match_result"
            load (convType $ C.typeOf (Match e cs)) result 0
        else
          genExpr e `withTerminator` \k eOpr -> mdo
            br switchBlock -- We need to end the current block before executing genCase
            -- 各ケースのコードとラベルを生成する
            -- switch用のタグがある場合は Right (タグ, ラベル) を、ない場合は Left タグ を返す
            (defaults, labels) <- partitionEithers <$> traverse (genCase eOpr (constructorList e) k) cs
            -- defaultsの先頭を取り出し、switchのデフォルトケースとする
            -- defaultsが空の場合、デフォルトケースはunreachableにジャンプする
            defaultLabel <- headDef (withBlock "match_default" unreachable) $ map pure defaults
            switchBlock <- withBlock "match_switch" do
              tagOpr <- case C.typeOf e of
                SumT _ -> do
                  tagAddr <- gep (innerType $ C.typeOf e) eOpr [int32 0, int32 0]
                  load i8 tagAddr 0
                RecordT _ -> pure $ int32 0 -- Tag value must be integer, so we use 0 as default value.
                _ -> pure eOpr
              switch tagOpr defaultLabel labels
            pure ()
genExpr (Switch v bs e) = do
  isAllocaResult <- asks (.isAllocaResult)
  if isAllocaResult
    then do
      result <- alloca (convType $ C.typeOf e) Nothing 0
      vOpr <- genAtom v

      lift $ mdo
        let cont = \opr -> do
              store result 0 opr
              br resultBlock
        br switchBlock
        labels <- traverse (genBranch (constructorList v) cont) bs
        defaultLabel <- withBlock "switch_default" do
          runContT (genExpr e) cont
        switchBlock <- withBlock "switch_switch" do
          tagOpr <- case C.typeOf v of
            SumT _ -> do
              tagAddr <- gep (innerType $ C.typeOf v) vOpr [int32 0, int32 0]
              load i8 tagAddr 0
            RecordT _ -> pure $ int32 0 -- Tag value must be integer, so we use 0 as default value.
            _ -> pure vOpr
          switch tagOpr defaultLabel labels
        pure ()
        resultBlock <- block `named` "switch_result"
        load (convType $ C.typeOf e) result 0
    else
      genAtom v `withTerminator` \k vOpr -> mdo
        br switchBlock
        labels <- traverse (genBranch (constructorList v) k) bs
        defaultLabel <- withBlock "switch_default" do
          runContT (genExpr e) k
        switchBlock <- withBlock "switch_switch" do
          tagOpr <- case C.typeOf v of
            SumT _ -> do
              tagAddr <- gep (innerType $ C.typeOf v) vOpr [int32 0, int32 0]
              load i8 tagAddr 0
            RecordT _ -> pure $ int32 0 -- Tag value must be integer, so we use 0 as default value.
            _ -> pure vOpr
          switch tagOpr defaultLabel labels
        pure ()
  where
    genBranch cs k (tag, e) = do
      let tag' = case C.typeOf v of
            SumT _ -> C.Int 8 $ fromIntegral $ Unsafe.fromJust $ List.findIndex (\(Con t _) -> tag == t) cs
            RecordT _ -> C.Int 8 0 -- Tag value must be integer, so we use 0 as default value.
            _ -> error "Switch is not supported for this type."
      label <- withBlock ("switch_branch_" <> encodeUtf8 (render (pPrint tag))) do
        runContT (genExpr e) k
      pure (tag', label)
genExpr (SwitchUnboxed v bs e) = do
  isAllocaResult <- asks (.isAllocaResult)
  if isAllocaResult
    then do
      result <- alloca (convType $ C.typeOf e) Nothing 0
      vOpr <- genAtom v

      lift mdo
        let cont = \opr -> do
              store result 0 opr
              br resultBlock
        br switchBlock
        labels <- traverse (genBranch cont) bs
        defaultLabel <- withBlock "switch-unboxed_default" do
          runContT (genExpr e) cont
        switchBlock <- withBlock "switch-unboxed_switch" do
          switch vOpr defaultLabel labels
        resultBlock <- block `named` "switch-unboxed_result"
        load (convType $ C.typeOf e) result 0
    else
      genAtom v `withTerminator` \k vOpr -> mdo
        br switchBlock
        labels <- traverse (genBranch k) bs
        defaultLabel <- withBlock "switch-unboxed_default" do
          runContT (genExpr e) k
        switchBlock <- withBlock "switch-unboxed_switch" do
          switch vOpr defaultLabel labels
        pure ()
  where
    genBranch k (u, e) = do
      ConstantOperand u' <- genAtom $ Unboxed u
      label <- withBlock ("switch-unboxed_branch_" <> encodeUtf8 (render (pPrint u))) do
        runContT (genExpr e) k
      pure (u', label)
genExpr (Destruct v (Con _ ts) xs e) = do
  v <- genAtom v
  payloadAddr <- gep (StructureType False [i8, StructureType False (map convType ts)]) v [int32 0, int32 1]
  env <-
    HashMap.fromList <$> ifor xs \i x -> do
      (x,) <$> do
        xAddr <- gep (StructureType False (map convType ts)) payloadAddr [int32 0, int32 $ fromIntegral i]
        load (convType $ C.typeOf x) xAddr 0
  local (over valueMap (env <>)) $ genExpr e
genExpr (DestructRecord scrutinee kvs e) = do
  scrutinee <- genAtom scrutinee
  kvs' <- for (HashMap.toList kvs) \(k, v) -> do
    hashTableGet <- findExt "malgo_hash_table_get" [ptr, ptr] ptr
    i <- getUniq
    key <- ConstantOperand <$> globalStringPtr k (mkName $ "key_" <> toString k <> show i)
    value <- call (FunctionType ptr [ptr, ptr] False) hashTableGet [(scrutinee, []), (key, [])]
    pure (v, value)
  local (over valueMap (HashMap.fromList kvs' <>)) $ genExpr e
genExpr (Assign _ v e) | C.typeOf v == VoidT = do
  _ <- genExpr v
  genExpr e
genExpr (Assign x v e) = do
  vOpr <- genExpr v
  local (over valueMap $ at x ?~ vOpr) $ genExpr e
genExpr (Error _) = ContT \_ -> unreachable

-- | Get constructor list from the type of scrutinee.
constructorList :: HasType s => s -> [Con]
constructorList scrutinee =
  case C.typeOf scrutinee of
    SumT cs -> cs
    _ -> []

genCase ::
  ( MonadCodeGen m,
    MonadIRBuilder m,
    MonadFail m,
    MonadFix m,
    MonadIO m
  ) =>
  Operand ->
  [Con] ->
  (Operand -> m ()) ->
  Case (Id C.Type) ->
  m (Either LLVM.AST.Name (C.Constant, LLVM.AST.Name))
genCase scrutinee cs k = \case
  Bind x _ e -> do
    label <- withBlock ("bind_" <> encodeUtf8 (render (pPrint x))) do
      local (over valueMap $ at x ?~ scrutinee) $ runContT (genExpr e) k
    pure $ Left label
  Exact u e -> do
    ConstantOperand u' <- genAtom $ Unboxed u
    label <- withBlock ("exact_" <> encodeUtf8 (render (pPrint u))) do
      runContT (genExpr e) k
    pure $ Right (u', label)
  Unpack con vs e -> do
    let (tag, conType) = genCon cs con
    label <- withBlock ("unpack_" <> encodeUtf8 (render (pPrint con))) do
      payloadAddr <- gep (StructureType False [i8, conType]) scrutinee [int32 0, int32 1]
      env <-
        HashMap.fromList <$> ifor vs \i v ->
          (v,) <$> do
            vAddr <- gep conType payloadAddr [int32 0, int32 $ fromIntegral i]
            load (convType $ C.typeOf v) vAddr 0
      local (over valueMap (env <>)) $ runContT (genExpr e) k
    pure $ Right (C.Int 8 tag, label)
  OpenRecord kvs e -> do
    label <- withBlock "open_record" do
      kvs' <- for (HashMap.toList kvs) $ \(k, v) -> do
        hashTableGet <- findExt "malgo_hash_table_get" [ptr, ptr] ptr
        i <- getUniq
        key <- ConstantOperand <$> globalStringPtr k (mkName $ "key_" <> toString k <> show i)
        value <- call (FunctionType ptr [ptr, ptr] False) hashTableGet [(scrutinee, []), (key, [])]
        pure (v, value)
      local (over valueMap (HashMap.fromList kvs' <>)) $ runContT (genExpr e) k
    pure $ Left label

genAtom ::
  (MonadCodeGen m, MonadIO m, MonadIRBuilder m) =>
  Atom (Id C.Type) ->
  m Operand
genAtom (Var x) = findVar x
genAtom (Unboxed (Int32 x)) = pure $ int32 x
genAtom (Unboxed (Int64 x)) = pure $ int64 x
-- Ref: https://github.com/llvm-hs/llvm-hs/issues/4
genAtom (Unboxed (Float x)) = pure $ ConstantOperand $ C.BitCast (C.Int 32 $ toInteger $ castFloatToWord32 x) LT.float
genAtom (Unboxed (Double x)) = pure $ ConstantOperand $ C.BitCast (C.Int 64 $ toInteger $ castDoubleToWord64 x) LT.double
genAtom (Unboxed (Char x)) = pure $ int8 $ toInteger $ ord x
genAtom (Unboxed (String x)) = do
  i <- getUniq
  ConstantOperand <$> globalStringPtr x (mkName $ "str" <> show i)
genAtom (Unboxed (Bool True)) = pure $ int8 1
genAtom (Unboxed (Bool False)) = pure $ int8 0

genLocalDef ::
  ( MonadCodeGen m,
    MonadIRBuilder m,
    MonadFail m,
    MonadFix m,
    MonadIO m
  ) =>
  LocalDef (Id C.Type) ->
  m (HashMap (Id C.Type) Operand)
genLocalDef (LocalDef funName _ (Fun ps e)) = do
  globalValues <- HashSet.fromList . HashMap.keys <$> view globalValueMap
  let fvs = toList $ freevars (Fun ps e) `HashSet.difference` globalValues
  -- キャプチャされる変数を詰める構造体の型
  let capType = StructureType False (map (convType . C.typeOf) fvs)
  -- クロージャの元になる関数を生成する
  name <- toName <$> newInternalId (funName.name <> "_closure") ()
  func <- internalFunction name (map (,NoParameterName) psTypes) retType $ \case
    [] -> error "The length of internal function parameters must be 1 or more"
    (capture : ps') -> do
      -- キャプチャした変数が詰まっている構造体を展開する
      env <-
        HashMap.fromList <$> ifor fvs \i fv ->
          (fv,) <$> do
            fvAddr <- gep capType capture [int32 0, int32 $ fromIntegral i] `named` (encodeUtf8 fv.name <> "_addr")
            load (convType $ C.typeOf fv) fvAddr 0 `named` encodeUtf8 fv.name
      local (over valueMap ((env <> HashMap.fromList (zip ps ps')) <>)) $ runContT (genExpr e) ret
  -- キャプチャされる変数を構造体に詰める
  capture <- mallocType capType `named` (encodeUtf8 funName.name <> "_capture")
  ifor_ fvs $ \i fv -> do
    fvOpr <- findVar fv
    gepAndStore capType capture [int32 0, int32 $ fromIntegral i] fvOpr `named` (encodeUtf8 fv.name)
  closAddr <- findVar funName
  gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 0] capture `named` (encodeUtf8 funName.name <> "_capture")
  gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 1] func `named` (encodeUtf8 funName.name <> "_func")
  pure $ one (funName, closAddr)
  where
    psTypes = ptr : map (convType . C.typeOf) ps
    retType = convType $ C.typeOf e
genLocalDef (LocalDef name@(C.typeOf -> SumT cs) _ (Pack _ con@(Con _ ts) xs)) = do
  addr <- mallocType (StructureType False [i8, StructureType False $ map convType ts])
  -- タグの書き込み
  gepAndStore (StructureType False [i8, StructureType False $ map convType ts]) addr [int32 0, int32 0] (int8 $ findIndex con cs)
  -- 引数の書き込み
  ifor_ xs $ \i x ->
    gepAndStore (StructureType False [i8, StructureType False $ map convType ts]) addr [int32 0, int32 1, int32 $ fromIntegral i] =<< genAtom x
  -- nameの型にキャスト
  pure $ HashMap.singleton name addr
genLocalDef (LocalDef (C.typeOf -> t) _ Pack {}) = error $ show t <> " must be SumT"
genLocalDef (LocalDef name _ (Record kvs)) = do
  newHashTable <- findExt "malgo_hash_table_new" [] ptr
  hashTable <- call (FunctionType ptr [] False) newHashTable []
  for_ (HashMap.toList kvs) \(k, v) -> do
    i <- getUniq
    k' <- ConstantOperand <$> globalStringPtr k (mkName $ "key_" <> toString k <> show i)
    v <- genAtom v
    insert <- findExt "malgo_hash_table_insert" [ptr, ptr, ptr] LT.void
    call (FunctionType LT.void [ptr, ptr, ptr] False) insert (map (,[]) [hashTable, k', v]) `named` ("hash_insert_" <> encodeUtf8 k)
  pure $ one (name, hashTable)

genCon :: [Con] -> Con -> (Integer, LT.Type)
genCon cs con@(Con _ ts)
  | con `elem` cs = (findIndex con cs, StructureType False (map convType ts))
  | otherwise = errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

findIndex :: (Pretty a, Eq a) => a -> [a] -> Integer
findIndex con cs = case List.elemIndex con cs of
  Just i -> fromIntegral i
  Nothing -> errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

globalStringPtr :: MonadModuleBuilder m => Text -> Name -> m C.Constant
globalStringPtr str nm = do
  let utf8Vals = map toInteger $ BL.unpack $ convertString str
      llvmVals = map (C.Int 8) (utf8Vals ++ [0])
      char = IntegerType 8
      charArray = C.Array char llvmVals
  ty <-
    LLVM.AST.Typed.typeOf charArray >>= \case
      Left err -> error $ show err
      Right ty -> pure ty
  emitDefn $
    GlobalDefinition
      globalVariableDefaults
        { LLVM.AST.Global.name = nm,
          LLVM.AST.Global.type' = ty,
          linkage = LLVM.AST.Linkage.External,
          isConstant = True,
          initializer = Just charArray,
          unnamedAddr = Just GlobalAddr
        }
  pure $ C.GetElementPtr True ty (C.GlobalReference nm) [C.Int 32 0, C.Int 32 0]

gepAndStore ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  LT.Type ->
  Operand ->
  [Operand] ->
  Operand ->
  m ()
gepAndStore ty opr addrs val = do
  addr <- gep ty opr addrs
  store addr 0 val

internalFunction ::
  MonadModuleBuilder m =>
  -- | Function name
  Name ->
  -- | Parameter types and name suggestions
  [(LT.Type, ParameterName)] ->
  -- | Return type
  LT.Type ->
  -- | Function body builder
  ([Operand] -> IRBuilderT m ()) ->
  m Operand
internalFunction label argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- for argtys $ \case
      (_, NoParameterName) -> fresh
      (_, ParameterName p) -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let def =
        GlobalDefinition
          functionDefaults
            { LLVM.AST.Global.name = label,
              linkage = LLVM.AST.Linkage.Internal,
              LLVM.AST.Global.parameters = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False),
              returnType = retty,
              basicBlocks = blocks
            }
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference label

withBlock :: MonadIRBuilder m => ShortByteString -> m () -> m Name
withBlock hint m = do
  label <- block `named` hint
  void m
  pure label
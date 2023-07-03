{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | LLVM Code Generator
module Koriel.Core.CodeGen.DelimLLVM
  ( codeGen,
  )
where

import Control.Lens (At (at), ifor, ifor_, makeFieldsNoPrefix, use, view, (<?=), (?=), (?~))
import Control.Monad.Cont (ContT, runContT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Cont (shiftT)
import Control.Monad.Trans.State.Lazy qualified as Lazy
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.Extra (headDef, mconcatMap)
import Data.Maybe qualified as Maybe
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
import LLVM.AST
  ( Definition (..),
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
import LLVM.AST.Type hiding
  ( double,
    void,
  )
import LLVM.AST.Type qualified as LT
import LLVM.AST.Typed (typeOf)
import LLVM.Context (withContext)
import LLVM.IRBuilder hiding (globalStringPtr, sizeof)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

data CodeGenState = CodeGenState
  { -- | 'primMap' is a map from primitive function name to its LLVM function.
    _primMap :: Map Name Operand,
    stringMap :: HashMap Text C.Constant
  }

makeFieldsNoPrefix ''CodeGenState

-- 変数のHashMapとknown関数のHashMapを分割する
-- #7(https://github.com/takoeight0821/malgo/issues/7)のようなバグの早期検出が期待できる
data CodeGenEnv = CodeGenEnv
  { uniqSupply :: UniqSupply,
    -- In optimization, some variables are defined multiple times.
    -- So, we need to treat them as as scoped variables.
    _valueMap :: HashMap (Id C.Type) Operand,
    _globalValueMap :: HashMap (Id C.Type) Operand,
    _funcMap :: HashMap (Id C.Type) Operand,
    moduleName :: ModuleName
  }

makeFieldsNoPrefix ''CodeGenEnv

newCodeGenEnv :: UniqSupply -> ModuleName -> Program (Id C.Type) -> CodeGenEnv
newCodeGenEnv uniqSupply moduleName Program {..} =
  CodeGenEnv
    { uniqSupply = uniqSupply,
      _valueMap = mempty,
      _globalValueMap = varMap,
      _funcMap = funcMap,
      moduleName = moduleName
    }
  where
    -- topVarsのOprMapを作成
    varMap = mconcatMap ?? topVars $ \(v, _, _) ->
      HashMap.singleton v (ConstantOperand $ C.GlobalReference $ toName v)
    -- topFuncsのOprMapを作成
    funcMap = mconcatMap ?? topFuns $ \(f, _, _, _) ->
      HashMap.singleton f (ConstantOperand $ C.GlobalReference $ toName f)

type MonadCodeGen m =
  ( MonadModuleBuilder m,
    MonadReader CodeGenEnv m,
    MonadState CodeGenState m
  ) ::
    Constraint

runCodeGenT :: (Monad m) => CodeGenEnv -> Lazy.StateT CodeGenState (ReaderT CodeGenEnv (ModuleBuilderT m)) a -> m [Definition]
runCodeGenT env m =
  execModuleBuilderT emptyModuleBuilder
    $ runReaderT (Lazy.evalStateT m $ CodeGenState mempty mempty) env

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
    genLoadModule $ runContT (initTopVars topVars) (const retVoid)
  let llvmModule =
        defaultModule
          { LLVM.AST.moduleName = fromString srcPath,
            moduleSourceFileName = fromString srcPath,
            moduleDefinitions = llvmir
          }
  liftIO $ withContext $ \ctx -> BS.writeFile dstPath =<< withModuleFromAST ctx llvmModule moduleLLVMAssembly
  where
    initTopVars [] = pass
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

sizeofCon :: (Num a) => Con -> a
sizeofCon (Con _ ts) = sum $ map sizeofType ts

sizeofType :: (Num a) => C.Type -> a
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
        Nothing -> findFuncVar
    findFuncVar =
      view (funcMap . at x) >>= \case
        Just opr -> do
          -- Generate a closure for the function
          let capture = ConstantOperand $ C.Null ptr
          closAddr <- mallocType (StructureType False [ptr, ptr])
          gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 0] capture `named` BS.toShort (convertString x.name <> "_capture")
          gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 1] opr `named` BS.toShort (convertString x.name <> "_func")
          pure closAddr
        Nothing -> findExtVar
    findExtVar =
      use (primMap . at (toName x)) >>= \case
        Just opr -> load (convType $ C.typeOf x) opr 0
        Nothing -> internExtVar
    internExtVar = do
      emitDefn
        $ GlobalDefinition
          globalVariableDefaults
            { LLVM.AST.Global.name = toName x,
              LLVM.AST.Global.type' = convType $ C.typeOf x,
              linkage = LLVM.AST.Linkage.External
            }
      let opr = ConstantOperand (C.GlobalReference (toName x))
      primMap . at (toName x) ?= opr
      load (convType $ C.typeOf x) opr 0

findFun :: (MonadCodeGen m) => Id C.Type -> m Operand
findFun x =
  view (funcMap . at x) >>= \case
    Just opr -> pure opr
    Nothing ->
      case C.typeOf x of
        ps :-> r -> findExt (toName x) (map convType ps) (convType r)
        _ -> error $ show $ pPrint x <> " is not found"

-- まだ生成していない外部関数を呼び出そうとしたら、externする
-- すでにexternしている場合は、そのOperandを返す
findExt :: (MonadCodeGen m) => Name -> [LT.Type] -> LT.Type -> m Operand
findExt x ps r =
  use (primMap . at x) >>= \case
    Just x -> pure x
    Nothing -> (primMap . at x <?=) =<< extern x ps r

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

toLabel :: (ConvertibleStrings s BS.ByteString) => s -> ShortByteString
toLabel s = BS.toShort $ convertString s

-- generate code for a toplevel variable definition
genVar :: (MonadModuleBuilder m) => Id C.Type -> Expr (Id C.Type) -> m Operand
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
    local (over valueMap (HashMap.fromList (zip params $ drop 1 args) <>)) $ runContT (genExpr body) ret
  where
    funcName = toName name
    llvmParams =
      (ptr, NoParameterName) -- capture pointer (not used)
        : map
          (\x -> (convType x.meta, ParameterName $ BS.toShort $ convertString $ idToText x))
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
  call
    (FunctionType (convType $ C.typeOf e) (ptr : map (convType . C.typeOf) xs) False)
    fOpr
    (map (,[]) (ConstantOperand (C.Null ptr) : xsOprs))
genExpr e@(RawCall name _ xs) = do
  let primOpr =
        ConstantOperand
          $ C.GlobalReference
          $ LLVM.AST.mkName
          $ convertString name
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
    i1ToBool i1opr = zext i1opr i8
    genCmpOp signed unsigned ordered x' y' =
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
      HashMap.singleton name
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
  | otherwise =
      shiftT \k -> do
        eOpr <- genExpr e
        tagOpr <- case C.typeOf e of
          SumT _ -> do
            tagAddr <- gep (innerType $ C.typeOf e) eOpr [int32 0, int32 0]
            load i8 tagAddr 0
          RecordT _ -> pure $ int32 0 -- Tag value must be integer, so we use 0 as default value.
          _ -> pure eOpr
        lift mdo
          switch tagOpr defaultLabel labels
          -- 各ケースのコードとラベルを生成する
          -- switch用のタグがある場合は Right (タグ, ラベル) を、ない場合は Left タグ を返す
          (defaults, labels) <- partitionEithers <$> traverse (genCase eOpr (constructorList e) k) cs
          -- defaultsの先頭を取り出し、switchのデフォルトケースとする
          -- defaultsが空の場合、デフォルトケースはunreachableにジャンプする
          defaultLabel <- headDef (withBlock ("match_default" :: BS.ByteString) unreachable) $ map pure defaults
          pass
genExpr (Switch v bs e) =
  shiftT \k -> do
    vOpr <- genAtom v
    tagOpr <- case C.typeOf v of
      SumT _ -> do
        tagAddr <- gep (innerType $ C.typeOf v) vOpr [int32 0, int32 0]
        load i8 tagAddr 0
      RecordT _ -> pure $ int32 0 -- Tag value must be integer, so we use 0 as default value.
      _ -> pure vOpr
    lift mdo
      switch tagOpr defaultLabel labels
      labels <- traverse (genBranch (constructorList v) k) bs
      defaultLabel <- withBlock ("switch_default" :: BS.ByteString) do
        runContT (genExpr e) k
      pass
  where
    genBranch cs k (tag, e) = do
      let tag' = case C.typeOf v of
            SumT _ -> C.Int 8 $ fromIntegral $ Maybe.fromJust $ List.findIndex (\(Con t _) -> tag == t) cs
            RecordT _ -> C.Int 8 0 -- Tag value must be integer, so we use 0 as default value.
            _ -> error "Switch is not supported for this type."
      label <- withBlock ("switch_branch_" <> render (pPrint tag)) do
        runContT (genExpr e) k
      pure (tag', label)
genExpr (SwitchUnboxed v bs e) =
  shiftT \k -> do
    vOpr <- genAtom v
    lift mdo
      switch vOpr defaultLabel labels
      labels <- traverse (genBranch k) bs
      defaultLabel <- withBlock ("switch-unboxed_default" :: BS.ByteString) do
        runContT (genExpr e) k
      pass
  where
    genBranch k (u, e) = do
      ConstantOperand u' <- genAtom $ Unboxed u
      label <- withBlock ("switch-unboxed_branch_" <> render (pPrint u)) do
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
    key <- ConstantOperand <$> globalStringPtr k
    value <- call (FunctionType ptr [ptr, ptr] False) hashTableGet [(scrutinee, []), (key, [])]
    pure (v, value)
  local (over valueMap (HashMap.fromList kvs' <>)) $ genExpr e
genExpr (Assign _ v e) | C.typeOf v == VoidT = do
  _ <- genExpr v
  genExpr e
genExpr (Assign x v e) = do
  vOpr <- genExpr v
  local (over valueMap $ at x ?~ vOpr) $ genExpr e
genExpr (Error _) = shiftT (const unreachable)

-- | Get constructor list from the type of scrutinee.
constructorList :: (HasType s) => s -> [Con]
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
    label <- withBlock ("bind_" <> render (pPrint x)) do
      local (over valueMap $ at x ?~ scrutinee) $ runContT (genExpr e) k
    pure $ Left label
  Exact u e -> do
    ConstantOperand u' <- genAtom $ Unboxed u
    label <- withBlock ("exact_" <> render (pPrint u)) do
      runContT (genExpr e) k
    pure $ Right (u', label)
  Unpack con vs e -> do
    let (tag, conType) = genCon cs con
    label <- withBlock ("unpack_" <> render (pPrint con)) do
      payloadAddr <- gep (StructureType False [i8, conType]) scrutinee [int32 0, int32 1]
      env <-
        HashMap.fromList <$> ifor vs \i v ->
          (v,) <$> do
            vAddr <- gep conType payloadAddr [int32 0, int32 $ fromIntegral i]
            load (convType $ C.typeOf v) vAddr 0
      local (over valueMap (env <>)) $ runContT (genExpr e) k
    pure $ Right (C.Int 8 tag, label)
  OpenRecord kvs e -> do
    label <- withBlock ("open_record" :: BS.ByteString) do
      kvs' <- for (HashMap.toList kvs) $ \(k, v) -> do
        hashTableGet <- findExt "malgo_hash_table_get" [ptr, ptr] ptr
        key <- ConstantOperand <$> globalStringPtr k
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
genAtom (Unboxed (String x)) = ConstantOperand <$> globalStringPtr x
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
            fvAddr <- gep capType capture [int32 0, int32 $ fromIntegral i] `named` toLabel (fv.name <> "_addr")
            load (convType $ C.typeOf fv) fvAddr 0 `named` toLabel fv.name
      local (over valueMap ((env <> HashMap.fromList (zip ps ps')) <>)) $ runContT (genExpr e) ret
  -- キャプチャされる変数を構造体に詰める
  capture <- mallocType capType `named` toLabel (funName.name <> "_capture")
  ifor_ fvs $ \i fv -> do
    fvOpr <- findVar fv
    gepAndStore capType capture [int32 0, int32 $ fromIntegral i] fvOpr `named` toLabel fv.name
  closAddr <- findVar funName
  gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 0] capture `named` toLabel (funName.name <> "_capture")
  gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 1] func `named` toLabel (funName.name <> "_func")
  pure $ HashMap.singleton funName closAddr
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
    k' <- ConstantOperand <$> globalStringPtr k
    v <- genAtom v
    insert <- findExt "malgo_hash_table_insert" [ptr, ptr, ptr] LT.void
    call (FunctionType LT.void [ptr, ptr, ptr] False) insert (map (,[]) [hashTable, k', v]) `named` toLabel ("hash_insert_" <> k)
  pure $ HashMap.singleton name hashTable

genCon :: [Con] -> Con -> (Integer, LT.Type)
genCon cs con@(Con _ ts)
  | con `elem` cs = (findIndex con cs, StructureType False (map convType ts))
  | otherwise = errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

findIndex :: (Pretty a, Eq a) => a -> [a] -> Integer
findIndex con cs = case List.elemIndex con cs of
  Just i -> fromIntegral i
  Nothing -> errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

globalStringPtr :: (MonadModuleBuilder m, MonadReader CodeGenEnv m, MonadState CodeGenState m, MonadIO m) => Text -> m C.Constant
globalStringPtr str = do
  stringMap <- gets (.stringMap)
  case HashMap.lookup str stringMap of
    Just strOpr -> pure strOpr
    Nothing -> do
      name <- mkName . ("str" <>) . show <$> getUniq
      let utf8Vals = map toInteger $ BL.unpack $ convertString str
          llvmVals = map (C.Int 8) (utf8Vals ++ [0])
          char = IntegerType 8
          charArray = C.Array char llvmVals
      ty <-
        LLVM.AST.Typed.typeOf charArray >>= \case
          Left err -> error $ show err
          Right ty -> pure ty
      emitDefn
        $ GlobalDefinition
          globalVariableDefaults
            { LLVM.AST.Global.name = name,
              LLVM.AST.Global.type' = ty,
              linkage = LLVM.AST.Linkage.External,
              isConstant = True,
              initializer = Just charArray,
              unnamedAddr = Just GlobalAddr
            }
      let opr = C.GetElementPtr True ty (C.GlobalReference name) [C.Int 32 0, C.Int 32 0]
      modify \s -> s {stringMap = HashMap.insert str opr s.stringMap}
      pure opr

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
  (MonadModuleBuilder m) =>
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

withBlock :: (MonadIRBuilder m, ConvertibleStrings s BS.ByteString) => s -> m () -> m Name
withBlock hint m = do
  label <- block `named` toLabel hint
  void m
  pure label
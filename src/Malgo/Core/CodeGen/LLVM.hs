{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrict #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | LLVM Code Generator
module Malgo.Core.CodeGen.LLVM
  ( codeGen,
  )
where

import Control.Lens (At (at), ifor, ifor_, makeFieldsNoPrefix, use, view, (<?=), (?=), (?~))
import Control.Monad.Cont (ContT, runContT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Cont (shiftT)
import Control.Monad.Trans.State.Lazy qualified as Lazy
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BS
import Data.List qualified as List
import Data.List.Extra (mconcatMap)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.String.Conversions
import Data.Traversable (for)
import Effectful (Eff, runPureEff)
import Effectful.Reader.Static qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import LLVM.AST
  ( Definition (..),
    Module (..),
    Name,
    defaultModule,
    mkName,
  )
import LLVM.AST.Constant qualified as C
import LLVM.AST.Global
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
import Malgo.Core.Syntax
import Malgo.Core.Type hiding (typeOf)
import Malgo.Core.Type qualified as C
import Malgo.Id
import Malgo.Module (ModuleName (..), moduleNameToString)
import Malgo.MonadUniq
import Malgo.Prelude

data CodeGenState = CodeGenState
  { -- | 'primMap' is a map from primitive function name to its LLVM function.
    _primMap :: Map Name Operand,
    stringMap :: Map Text C.Constant,
    uniqSupply :: Int
  }

makeFieldsNoPrefix ''CodeGenState

-- 変数のMapとknown関数のMapを分割する
-- #7(https://github.com/takoeight0821/malgo/issues/7)のようなバグの早期検出が期待できる
data CodeGenEnv = CodeGenEnv
  { -- In optimization, some variables are defined multiple times.
    -- So, we need to treat them as as scoped variables.
    _valueMap :: Map (Meta C.Type) Operand,
    _globalValueMap :: Map (Meta C.Type) Operand,
    _funcMap :: Map (Meta C.Type) Operand,
    moduleName :: ModuleName
  }

makeFieldsNoPrefix ''CodeGenEnv

newCodeGenEnv :: ModuleName -> Program (Meta C.Type) -> CodeGenEnv
newCodeGenEnv moduleName Program {..} =
  CodeGenEnv
    { _valueMap = mempty,
      _globalValueMap = varMap,
      _funcMap = funcMap,
      moduleName
    }
  where
    -- topVarsのOprMapを作成
    varMap = mconcatMap ?? topVars $ \(v, _, _) ->
      Map.singleton v (ConstantOperand $ C.GlobalReference $ toName v)
    -- topFuncsのOprMapを作成
    funcMap = mconcatMap ?? topFuns $ \(f, _, _, _) ->
      Map.singleton f (ConstantOperand $ C.GlobalReference $ toName f)

type MonadCodeGen m =
  ( MonadModuleBuilder m,
    MonadReader CodeGenEnv m,
    MonadState CodeGenState m
  ) ::
    Constraint

runCodeGenT :: (Monad m) => Int -> CodeGenEnv -> Lazy.StateT CodeGenState (ReaderT CodeGenEnv (ModuleBuilderT m)) a -> m [Definition]
runCodeGenT n env m =
  execModuleBuilderT emptyModuleBuilder
    $ runReaderT (Lazy.evalStateT m $ CodeGenState mempty mempty n) env

-- | Generate LLVM IR from a program.
codeGen ::
  (MonadFix m, MonadIO m) =>
  -- | Source file path
  FilePath ->
  -- | Destination file path
  FilePath ->
  -- | Module name of the source program
  ModuleName ->
  -- | Entry point of the source program
  Maybe (Meta C.Type) ->
  Int ->
  -- | Source program
  Program (Meta C.Type) ->
  m ()
codeGen srcPath dstPath modName mentry n Program {..} = do
  llvmir <- runCodeGenT n (newCodeGenEnv modName Program {..}) do
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
          runEffOnCodeGen
            $ mainFunc
            =<< runDef do
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
        Nothing -> error $ show $ pretty name <+> "is not found"
        Just name' -> do
          eOpr <- genExpr expr
          store name' 0 eOpr
          initTopVars xs
    -- Generate main function.
    mainFunc e = do
      -- `Builtin.main` are compiled as `main` in `Malgo.Core.CodeGen.toName`
      mainFuncId <- withMeta ([] :-> Int32T) <$> newNativeId "main"
      mainFuncBody <- runDef do
        _ <- bind $ RawCall "GC_init" ([] :-> VoidT) []
        _ <- bind $ RawCall ("malgo_load_" <> moduleNameToString modName) ([] :-> VoidT) []
        pure e
      pure (mainFuncId, ([], mainFuncBody))

runEffOnCodeGen :: (MonadState CodeGenState m, MonadReader CodeGenEnv m) => Eff '[Eff.Reader ModuleName, Eff.State Uniq] a -> m a
runEffOnCodeGen e = do
  moduleName <- asks (.moduleName)
  n <- gets (.uniqSupply)
  let (e', Uniq n') = runPureEff do
        Eff.runState (Uniq n) (Eff.runReader moduleName e)
  modify \s -> s {uniqSupply = n'}
  pure e'

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

findVar :: (MonadCodeGen m, MonadIRBuilder m) => Meta C.Type -> m Operand
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
          gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 0] capture `named` BS.toShort (convertString x.id.name <> "_capture")
          gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 1] opr `named` BS.toShort (convertString x.id.name <> "_func")
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

findFun :: (MonadCodeGen m) => Meta C.Type -> m Operand
findFun x =
  view (funcMap . at x) >>= \case
    Just opr -> pure opr
    Nothing ->
      case C.typeOf x of
        ps :-> r -> findExt (toName x) (map convType ps) (convType r)
        _ -> error $ show $ pretty x <> " is not found"

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

toName :: Meta a -> LLVM.AST.Name
toName meta = LLVM.AST.mkName $ convertString $ idToText meta.id

toLabel :: (ConvertibleStrings s BS.ByteString) => s -> ShortByteString
toLabel s = BS.toShort $ convertString s

-- generate code for a toplevel variable definition
genVar :: (MonadModuleBuilder m) => Meta C.Type -> Expr (Meta C.Type) -> m Operand
genVar name expr = global (toName name) (convType $ C.typeOf expr) (C.Undef (convType $ C.typeOf expr))

genLoadModule :: (MonadModuleBuilder m, MonadReader CodeGenEnv m) => IRBuilderT m () -> m Operand
genLoadModule m = do
  modName <- asks (.moduleName)
  internalFunction (LLVM.AST.mkName $ "malgo_load_" <> moduleNameToString modName) [] LT.void $ const m

-- generate code for a 'known' function
genFunc ::
  ( MonadCodeGen m,
    MonadFix m,
    MonadIO m
  ) =>
  Meta C.Type ->
  [Meta C.Type] ->
  Expr (Meta C.Type) ->
  m Operand
genFunc name params body = do
  let funcBuilder =
        if idIsNative name.id
          then function
          else internalFunction
  funcBuilder funcName llvmParams retty $ \args ->
    local (over valueMap (Map.fromList (zip params $ drop 1 args) <>)) $ runContT (genExpr body) ret
  where
    funcName = toName name
    llvmParams =
      (ptr, NoParameterName) -- capture pointer (not used)
        : map
          (\x -> (convType x.meta, ParameterName $ BS.toShort $ convertString $ idToText x.id))
          params
    retty = convType (C.typeOf body)

genExpr ::
  ( MonadIRBuilder m,
    MonadCodeGen m,
    MonadFix m,
    MonadIO m
  ) =>
  Expr (Meta C.Type) ->
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
      Map.singleton name
        <$> mallocType (StructureType False [ptr, ptr])
    prepare _ = pure mempty
genExpr Match {} = error "unreachable: Match must be normalized before codegen."
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
      label <- withBlock ("switch_branch_" <> render (pretty tag)) do
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
      genAtom (Unboxed u) >>= \case
        ConstantOperand u' -> do
          label <- withBlock ("switch-unboxed_branch_" <> render (pretty u)) do
            runContT (genExpr e) k
          pure (u', label)
        _ -> error "unreachable: genExpr"
genExpr (Destruct v (Con _ ts) xs e) = do
  v <- genAtom v
  payloadAddr <- gep (StructureType False [i8, StructureType False (map convType ts)]) v [int32 0, int32 1]
  env <-
    Map.fromList <$> ifor xs \i x -> do
      (x,) <$> do
        xAddr <- gep (StructureType False (map convType ts)) payloadAddr [int32 0, int32 $ fromIntegral i]
        load (convType $ C.typeOf x) xAddr 0
  local (over valueMap (env <>)) $ genExpr e
genExpr (DestructRecord scrutinee kvs e) = do
  scrutinee <- genAtom scrutinee
  kvs' <- for (Map.toList kvs) \(k, v) -> do
    hashTableGet <- findExt "malgo_hash_table_get" [ptr, ptr] ptr
    key <- ConstantOperand <$> globalStringPtr k
    value <- call (FunctionType ptr [ptr, ptr] False) hashTableGet [(scrutinee, []), (key, [])]
    pure (v, value)
  local (over valueMap (Map.fromList kvs' <>)) $ genExpr e
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

genAtom ::
  (MonadCodeGen m, MonadIRBuilder m) =>
  Atom (Meta C.Type) ->
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
    MonadFix m,
    MonadIO m
  ) =>
  LocalDef (Meta C.Type) ->
  m (Map (Meta C.Type) Operand)
genLocalDef (LocalDef funName _ (Fun ps e)) = do
  globalValues <- Set.fromList . Map.keys <$> view globalValueMap
  let fvs = toList $ freevars (Fun ps e) `Set.difference` globalValues
  -- キャプチャされる変数を詰める構造体の型
  let capType = StructureType False (map (convType . C.typeOf) fvs)
  -- クロージャの元になる関数を生成する
  name <- toName <$> runEffOnCodeGen (withMeta () <$> newInternalId (funName.id.name <> "_closure"))
  func <- internalFunction name (map (,NoParameterName) psTypes) retType $ \case
    [] -> error "The length of internal function parameters must be 1 or more"
    (capture : ps') -> do
      -- キャプチャした変数が詰まっている構造体を展開する
      env <-
        Map.fromList <$> ifor fvs \i fv ->
          (fv,) <$> do
            fvAddr <- gep capType capture [int32 0, int32 $ fromIntegral i] `named` toLabel (fv.id.name <> "_addr")
            load (convType $ C.typeOf fv) fvAddr 0 `named` toLabel fv.id.name
      local (over valueMap ((env <> Map.fromList (zip ps ps')) <>)) $ runContT (genExpr e) ret
  -- キャプチャされる変数を構造体に詰める
  capture <- mallocType capType `named` toLabel (funName.id.name <> "_capture")
  ifor_ fvs $ \i fv -> do
    fvOpr <- findVar fv
    gepAndStore capType capture [int32 0, int32 $ fromIntegral i] fvOpr `named` toLabel fv.id.name
  closAddr <- findVar funName
  gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 0] capture `named` toLabel (funName.id.name <> "_capture")
  gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 1] func `named` toLabel (funName.id.name <> "_func")
  pure $ Map.singleton funName closAddr
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
  pure $ Map.singleton name addr
genLocalDef (LocalDef (C.typeOf -> t) _ Pack {}) = error $ show t <> " must be SumT"
genLocalDef (LocalDef name _ (Record kvs)) = do
  newHashTable <- findExt "malgo_hash_table_new" [] ptr
  hashTable <- call (FunctionType ptr [] False) newHashTable []
  for_ (Map.toList kvs) \(k, v) -> do
    k' <- ConstantOperand <$> globalStringPtr k
    v <- genAtom v
    insert <- findExt "malgo_hash_table_insert" [ptr, ptr, ptr] LT.void
    call (FunctionType LT.void [ptr, ptr, ptr] False) insert (map (,[]) [hashTable, k', v]) `named` toLabel ("hash_insert_" <> k)
  pure $ Map.singleton name hashTable

findIndex :: (Pretty a, Eq a) => a -> [a] -> Integer
findIndex con cs = case List.elemIndex con cs of
  Just i -> fromIntegral i
  Nothing -> errorDoc $ pretty con <+> "is not in" <+> pretty cs

globalStringPtr :: (MonadModuleBuilder m, MonadReader CodeGenEnv m, MonadState CodeGenState m) => Text -> m C.Constant
globalStringPtr str = do
  stringMap <- gets (.stringMap)
  case Map.lookup str stringMap of
    Just strOpr -> pure strOpr
    Nothing -> do
      name <- mkName . ("str" <>) . show <$> runEffOnCodeGen getUniq
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
      modify \s -> s {stringMap = Map.insert str opr s.stringMap}
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
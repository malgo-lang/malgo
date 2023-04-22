{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | LLVM Code Generator
module Koriel.Core.CodeGen.LLVM
  ( codeGen,
  )
where

import Control.Lens (At (at), ifor, ifor_, makeFieldsNoPrefix, use, view, (<?=), (?=), (?~))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.State.Lazy qualified as Lazy
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BS (toShort)
import Data.Char (ord)
import Data.Either (partitionEithers)
import Data.HashMap.Strict qualified as HashMap
import Data.Kind (Constraint)
import Data.List qualified as List
import Data.List.Extra (headDef, mconcatMap)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.String.Conversions
import Data.Traversable (for)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import GHC.Records (HasField)
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
import Malgo.Monad (MalgoEnv (..))

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
    moduleName :: ModuleName
  }

makeFieldsNoPrefix ''CodeGenEnv

newCodeGenEnv :: (HasUniqSupply r) => r -> ModuleName -> Program (Id C.Type) -> CodeGenEnv
newCodeGenEnv malgoEnv moduleName Program {..} =
  CodeGenEnv
    { uniqSupply = malgoEnv.uniqSupply,
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
    MonadState PrimMap m
  ) ::
    Constraint

runCodeGenT :: (Monad m) => CodeGenEnv -> Lazy.StateT PrimMap (ReaderT CodeGenEnv (ModuleBuilderT m)) a -> m [Definition]
runCodeGenT env m =
  execModuleBuilderT emptyModuleBuilder $
    runReaderT (Lazy.evalStateT m mempty) env

-- | Generate LLVM IR from a program.
codeGen ::
  ( MonadFix m,
    MonadFail m,
    MonadIO m,
    HasField "_nameEnv" d (HashMap (Id ()) (Id C.Type))
  ) =>
  -- | Source file path
  FilePath ->
  -- | Malgo environment
  MalgoEnv ->
  -- | Module name of the source program
  ModuleName ->
  -- | Collected information from the desugarer
  d ->
  -- | Source program
  Program (Id C.Type) ->
  m ()
codeGen srcPath malgoEnv modName dsState Program {..} = do
  llvmir <- runCodeGenT (newCodeGenEnv malgoEnv modName Program {..}) do
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
    case searchMain (HashMap.toList dsState._nameEnv) of
      Just mainCall -> do
        (f, (ps, body)) <-
          mainFunc =<< runDef do
            let unitCon = C.Con C.Tuple []
            unit <- let_ (SumT [unitCon]) (Pack (SumT [unitCon]) unitCon [])
            _ <- bind $ mainCall [unit]
            pure (Atom $ Unboxed $ Int32 0)
        void $ genFunc f ps body
      Nothing -> pass
    genLoadModule $ initTopVars topVars
  let llvmModule =
        defaultModule
          { LLVM.AST.moduleName = BS.toShort $ convertString srcPath,
            moduleSourceFileName = BS.toShort $ convertString srcPath,
            moduleDefinitions = llvmir
          }
  liftIO $ withContext $ \ctx -> BS.writeFile malgoEnv.dstPath =<< withModuleFromAST ctx llvmModule moduleLLVMAssembly
  where
    initTopVars [] = retVoid
    initTopVars ((name, _, expr) : xs) =
      view (globalValueMap . at name) >>= \case
        Nothing -> error $ show $ pPrint name <+> "is not found"
        Just name' -> genExpr expr \eOpr -> do
          -- TODO[genExpr does not return correctly-typed value]
          -- eOpr <- bitcast eOpr (convType (C.typeOf name))
          store name' 0 eOpr
          initTopVars xs
    -- エントリーポイントとなるmain関数を検索する
    searchMain :: [(Id a, Id b)] -> Maybe ([Atom (Id b)] -> Expr (Id b))
    searchMain ((griffId@Id {sort = Koriel.Id.External}, coreId) : _)
      | griffId.name == "main"
          && griffId.moduleName == modName =
          Just $ CallDirect coreId
    searchMain (_ : xs) = searchMain xs
    searchMain _ = Nothing
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
genVar :: (MonadModuleBuilder m) => Id C.Type -> Expr (Id C.Type) -> m Operand
genVar name expr = global (toName name) (convType $ C.typeOf expr) (C.Undef (convType $ C.typeOf expr))

genLoadModule :: (MonadModuleBuilder m, MonadReader CodeGenEnv m) => IRBuilderT m () -> m Operand
genLoadModule m = do
  ModuleName modName <- asks (.moduleName)
  function (LLVM.AST.mkName $ convertString $ "koriel_load_" <> modName) [] LT.void $ const m

-- generate code for a 'known' function
genFunc ::
  ( MonadModuleBuilder m,
    MonadReader CodeGenEnv m,
    MonadState PrimMap m,
    MonadFix m,
    MonadFail m,
    MonadIO m
  ) =>
  Id C.Type ->
  [Id C.Type] ->
  Expr (Id C.Type) ->
  m Operand
genFunc name params body
  | idIsExternal name || idIsNative name = do
      moduleName <- asks (.moduleName)
      if name.moduleName == moduleName
        then function funcName llvmParams retty $ \args -> local (over valueMap (HashMap.fromList (zip params args) <>)) $ genExpr body ret
        else internalFunction funcName llvmParams retty $ \args -> local (over valueMap (HashMap.fromList (zip params args) <>)) $ genExpr body ret
  | otherwise =
      internalFunction funcName llvmParams retty $ \args -> local (over valueMap (HashMap.fromList (zip params args) <>)) $ genExpr body ret
  where
    funcName = toName name
    llvmParams =
      map
        (\x -> (convType $ x.meta, ParameterName $ BS.toShort $ convertString $ idToText x))
        params
    retty = convType (C.typeOf body)

genExpr ::
  ( MonadReader CodeGenEnv m,
    MonadState PrimMap m,
    MonadIRBuilder m,
    MonadModuleBuilder m,
    MonadFail m,
    MonadFix m,
    MonadIO m
  ) =>
  Expr (Id C.Type) ->
  (Operand -> m ()) ->
  m ()
genExpr e k = genExp' e \eOpr -> do
  case C.typeOf e of
    VoidT -> k (ConstantOperand (C.Undef LT.void))
    t
      | convType t /= ptr -> k =<< bitcast eOpr (convType t)
      | otherwise -> k eOpr

-- genUnpackでコード生成しつつラベルを返すため、CPSにしている
genExp' ::
  ( MonadReader CodeGenEnv m,
    MonadState PrimMap m,
    MonadIRBuilder m,
    MonadModuleBuilder m,
    MonadFail m,
    MonadFix m,
    MonadIO m
  ) =>
  Expr (Id C.Type) ->
  (Operand -> m ()) ->
  m ()
genExp' (Atom x) k = k =<< genAtom x
genExp' e@(Call f xs) k = do
  fOpr <- genAtom f
  captureAddr <- gep (innerType $ C.typeOf f) fOpr [int32 0, int32 0]
  captureOpr <- load ptr captureAddr 0
  funcAddr <- gep (innerType $ C.typeOf f) fOpr [int32 0, int32 1]
  funcOpr <- load ptr funcAddr 0
  xsOprs <- traverse genAtom xs
  k =<< call (FunctionType (convType $ C.typeOf e) (map (convType . C.typeOf) xs) False) funcOpr (map (,[]) $ captureOpr : xsOprs)
genExp' e@(CallDirect f xs) k = do
  fOpr <- findFun f
  xsOprs <- traverse genAtom xs
  k =<< call (FunctionType (convType $ C.typeOf e) (map (convType . C.typeOf) xs) False) fOpr (map (,[]) xsOprs)
genExp' e@(RawCall name _ xs) k = do
  let primOpr =
        ConstantOperand $
          C.GlobalReference $
            LLVM.AST.mkName $
              convertString name
  xsOprs <- traverse genAtom xs
  k =<< call (FunctionType (convType $ C.typeOf e) (map (convType . C.typeOf) xs) False) primOpr (map (,[]) xsOprs)
genExp' (BinOp o x y) k = k =<< join (genOp o <$> genAtom x <*> genAtom y)
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
    genOp Op.Eq = \x' y' ->
      i1ToBool =<< case C.typeOf x of
        Int32T -> icmp IP.EQ x' y'
        Int64T -> icmp IP.EQ x' y'
        FloatT -> fcmp FP.OEQ x' y'
        DoubleT -> fcmp FP.OEQ x' y'
        CharT -> icmp IP.EQ x' y'
        StringT -> icmp IP.EQ x' y'
        BoolT -> icmp IP.EQ x' y'
        SumT _ -> icmp IP.EQ x' y'
        t -> error $ show t <> " is not comparable"
    genOp Op.Neq = \x' y' ->
      i1ToBool =<< case C.typeOf x of
        Int32T -> icmp IP.NE x' y'
        Int64T -> icmp IP.NE x' y'
        FloatT -> fcmp FP.ONE x' y'
        DoubleT -> fcmp FP.ONE x' y'
        CharT -> icmp IP.NE x' y'
        StringT -> icmp IP.NE x' y'
        BoolT -> icmp IP.NE x' y'
        SumT _ -> icmp IP.NE x' y'
        t -> error $ show t <> " is not comparable"
    genOp Op.Lt = \x' y' ->
      i1ToBool =<< case C.typeOf x of
        Int32T -> icmp IP.SLT x' y'
        Int64T -> icmp IP.SLT x' y'
        FloatT -> fcmp FP.OLT x' y'
        DoubleT -> fcmp FP.OLT x' y'
        CharT -> icmp IP.ULT x' y'
        t -> error $ show t <> " is not comparable"
    genOp Op.Le = \x' y' ->
      i1ToBool =<< case C.typeOf x of
        Int32T -> icmp IP.SLE x' y'
        Int64T -> icmp IP.SLE x' y'
        FloatT -> fcmp FP.OLE x' y'
        DoubleT -> fcmp FP.OLE x' y'
        CharT -> icmp IP.ULE x' y'
        t -> error $ show t <> " is not comparable"
    genOp Op.Gt = \x' y' ->
      i1ToBool =<< case C.typeOf x of
        Int32T -> icmp IP.SGT x' y'
        Int64T -> icmp IP.SGT x' y'
        FloatT -> fcmp FP.OGT x' y'
        DoubleT -> fcmp FP.OGT x' y'
        CharT -> icmp IP.UGT x' y'
        t -> error $ show t <> " is not comparable"
    genOp Op.Ge = \x' y' ->
      i1ToBool =<< case C.typeOf x of
        Int32T -> icmp IP.SGE x' y'
        Int64T -> icmp IP.SGE x' y'
        FloatT -> fcmp FP.OGE x' y'
        DoubleT -> fcmp FP.OGE x' y'
        CharT -> icmp IP.UGE x' y'
        t -> error $ show t <> " is not comparable"
    genOp Op.And = LLVM.IRBuilder.and
    genOp Op.Or = LLVM.IRBuilder.or
    i1ToBool i1opr = zext i1opr i8
genExp' (Cast ty x) k = do
  xOpr <- genAtom x
  xOprTy <- typeOf xOpr
  if Right (convType ty) /= xOprTy
    then k =<< bitcast xOpr (convType ty)
    else k xOpr
genExp' (Let xs e) k = do
  env <- foldMapM prepare xs
  env <- local (over valueMap (env <>)) $ mconcat <$> traverse genLocalDef xs
  local (over valueMap (env <>)) $ genExpr e k
  where
    -- Generate a `malloc(sizeof(<closure type>))` call for a local function definition.
    prepare (LocalDef name _ (Fun _ _)) =
      HashMap.singleton name
        <$> mallocType (StructureType False [ptr, ptr])
    prepare _ = pure mempty
-- These `match` cases are not necessary.
genExp' (Match e (Bind _ _ body : _)) k | C.typeOf e == VoidT = genExpr e $ \_ -> genExpr body k
genExp' (Match e (Bind x _ body : _)) k = genExpr e $ \eOpr -> do
  local (over valueMap (at x ?~ eOpr)) (genExpr body k)
genExp' (Match e cs) k
  | C.typeOf e == VoidT = error "VoidT is not able to bind to variable."
  | otherwise = genExpr e $ \eOpr -> mdo
      -- eOprの型がptr i8だったときに正しくタグを取り出すため、bitcastする
      -- TODO[genExpr does not return correctly-typed value]
      -- genExpが正しい型の値を継続に渡すように変更する
      -- eOpr' <- bitcast eOpr (convType $ C.typeOf e)
      br switchBlock -- We need to end the current block before executing genCase
      -- 各ケースのコードとラベルを生成する
      -- switch用のタグがある場合は Right (タグ, ラベル) を、ない場合は Left タグ を返す
      (defaults, labels) <- partitionEithers . toList <$> traverse (genCase eOpr (constructorList e) k) cs
      -- defaultsの先頭を取り出し、switchのデフォルトケースとする
      -- defaultsが空の場合、デフォルトケースはunreachableにジャンプする
      defaultLabel <- headDef (block >>= \l -> unreachable >> pure l) $ map pure defaults
      switchBlock <- block
      tagOpr <- case C.typeOf e of
        SumT _ -> do
          tagAddr <- gep (innerType $ C.typeOf e) eOpr [int32 0, int32 0]
          load i8 tagAddr 0
        RecordT _ -> pure $ int32 0 -- Tag value must be integer, so we use 0 as default value.
        _ -> pure eOpr
      switch tagOpr defaultLabel labels
genExp' (Switch v bs e) k = mdo
  vOpr <- genAtom v
  br switchBlock
  labels <- toList <$> traverse (genBranch (constructorList v) k) bs
  defaultLabel <- block
  genExpr e k
  switchBlock <- block
  tagOpr <- case C.typeOf v of
    SumT _ -> do
      tagAddr <- gep (innerType $ C.typeOf v) vOpr [int32 0, int32 0]
      load i8 tagAddr 0
    RecordT _ -> pure $ int32 0 -- Tag value must be integer, so we use 0 as default value.
    _ -> pure vOpr
  switch tagOpr defaultLabel labels
  where
    genBranch cs k (tag, e) = do
      label <- block
      let tag' = case C.typeOf v of
            SumT _ -> C.Int 8 $ fromIntegral $ fromJust $ List.findIndex (\(Con t _) -> tag == t) cs
            RecordT _ -> C.Int 8 0 -- Tag value must be integer, so we use 0 as default value.
            _ -> error "Switch is not supported for this type."
      genExpr e k
      pure (tag', label)
genExp' (SwitchUnboxed v bs e) k = mdo
  vOpr <- genAtom v
  br switchBlock
  labels <- toList <$> traverse (genBranch k) bs
  defaultLabel <- block
  genExpr e k
  switchBlock <- block
  switch vOpr defaultLabel labels
  where
    genBranch k (u, e) = do
      ConstantOperand u' <- genAtom $ Unboxed u
      label <- block
      genExpr e k
      pure (u', label)
genExp' (Destruct v (Con _ ts) xs e) k = do
  v <- genAtom v
  payloadAddr <- gep (StructureType False [i8, StructureType False (map convType ts)]) v [int32 0, int32 1]
  env <-
    HashMap.fromList <$> ifor xs \i x -> do
      (x,) <$> do
        xAddr <- gep (StructureType False (map convType ts)) payloadAddr [int32 0, int32 $ fromIntegral i]
        load (convType $ C.typeOf x) xAddr 0
  local (over valueMap (env <>)) $ genExpr e k
genExp' (DestructRecord scrutinee kvs e) k = do
  scrutinee <- (bitcast ?? convType (C.typeOf scrutinee)) =<< genAtom scrutinee
  kvs' <- for (HashMap.toList kvs) \(k, v) -> do
    hashTableGet <- findExt "malgo_hash_table_get" [ptr, ptr] ptr
    i <- getUniq
    key <- ConstantOperand <$> globalStringPtr k (mkName $ "key_" <> convertString k <> show i)
    value <- call (FunctionType ptr [ptr, ptr] False) hashTableGet [(scrutinee, []), (key, [])]
    value <- bitcast value (convType $ C.typeOf v)
    pure (v, value)
  local (over valueMap (HashMap.fromList kvs' <>)) $ genExpr e k
genExp' (Assign _ v e) k | C.typeOf v == VoidT = genExpr v $ \_ -> genExpr e k
genExp' (Assign x v e) k = do
  genExpr v $ \vOpr -> do
    vOpr <- bitcast vOpr (convType $ C.typeOf v)
    local (over valueMap $ at x ?~ vOpr) $ genExpr e k
genExp' (Error _) _ = unreachable

-- | Get constructor list from the type of scrutinee.
constructorList :: (HasType s) => s -> [Con]
constructorList scrutinee =
  case C.typeOf scrutinee of
    SumT cs -> cs
    _ -> []

genCase ::
  ( MonadReader CodeGenEnv m,
    MonadModuleBuilder m,
    MonadState PrimMap m,
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
    label <- block
    void $ local (over valueMap $ at x ?~ scrutinee) $ genExpr e k
    pure $ Left label
  Exact u e -> do
    ConstantOperand u' <- genAtom $ Unboxed u
    label <- block
    genExpr e k
    pure $ Right (u', label)
  Unpack con vs e -> do
    label <- block
    let (tag, conType) = genCon cs con
    -- addr <- bitcast scrutinee (ptr $ StructureType False [i8, conType])
    payloadAddr <- gep (StructureType False [i8, conType]) scrutinee [int32 0, int32 1]
    -- WRONG: payloadAddr <- (bitcast ?? ptr conType) =<< gep scrutinee [int32 0, int32 1]
    env <-
      HashMap.fromList <$> ifor vs \i v ->
        (v,) <$> do
          vAddr <- gep conType payloadAddr [int32 0, int32 $ fromIntegral i]
          load (convType $ C.typeOf v) vAddr 0
    void $ local (over valueMap (env <>)) $ genExpr e k
    pure $ Right (C.Int 8 tag, label)
  OpenRecord kvs e -> do
    label <- block
    kvs' <- for (HashMap.toList kvs) $ \(k, v) -> do
      hashTableGet <- findExt "malgo_hash_table_get" [ptr, ptr] ptr
      i <- getUniq
      key <- ConstantOperand <$> globalStringPtr k (mkName $ "key_" <> convertString k <> show i)
      value <- call (FunctionType ptr [ptr, ptr] False) hashTableGet [(scrutinee, []), (key, [])]
      value <- bitcast value (convType $ C.typeOf v)
      pure (v, value)
    local (over valueMap (HashMap.fromList kvs' <>)) $ genExpr e k
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
  -- クロージャの元になる関数を生成する
  name <- toName <$> newInternalId (funName.name <> "_closure") ()
  func <- internalFunction name (map (,NoParameterName) psTypes) retType $ \case
    [] -> error "The length of internal function parameters must be 1 or more"
    (capture : ps') -> do
      -- キャプチャした変数が詰まっている構造体を展開する
      -- capture <- bitcast rawCapture (ptr capType)
      env <-
        HashMap.fromList <$> ifor fvs \i fv ->
          (fv,) <$> do
            fvAddr <- gep capType capture [int32 0, int32 $ fromIntegral i]
            load (convType $ C.typeOf fv) fvAddr 0
      local (over valueMap ((env <> HashMap.fromList (zip ps ps')) <>)) $ genExpr e ret
  -- キャプチャされる変数を構造体に詰める
  capture <- mallocType capType
  ifor_ fvs $ \i fv -> do
    fvOpr <- findVar fv
    gepAndStore capType capture [int32 0, int32 $ fromIntegral i] fvOpr
  closAddr <- findVar funName
  gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 0] capture
  gepAndStore (StructureType False [ptr, ptr]) closAddr [int32 0, int32 1] func
  pure $ HashMap.singleton funName closAddr
  where
    fvs = toList $ freevars (Fun ps e)
    -- キャプチャされる変数を詰める構造体の型
    capType = StructureType False (map (convType . C.typeOf) fvs)
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
  HashMap.singleton name <$> bitcast addr (convType $ SumT cs)
genLocalDef (LocalDef (C.typeOf -> t) _ Pack {}) = error $ show t <> " must be SumT"
genLocalDef (LocalDef name _ (Record kvs)) = do
  newHashTable <- findExt "malgo_hash_table_new" [] ptr
  hashTable <- call (FunctionType ptr [] False) newHashTable []
  for_ (HashMap.toList kvs) \(k, v) -> do
    i <- getUniq
    k' <- ConstantOperand <$> globalStringPtr k (mkName $ "key_" <> convertString k <> show i)
    v <- join $ bitcast <$> genAtom v <*> pure ptr
    insert <- findExt "malgo_hash_table_insert" [ptr, ptr, ptr] LT.void
    call (FunctionType LT.void [ptr, ptr, ptr] False) insert (map (,[]) [hashTable, k', v]) `named` ("hash_insert_" <> BS.toShort (convertString k))
  pure $ HashMap.singleton name hashTable

genCon :: [Con] -> Con -> (Integer, LT.Type)
genCon cs con@(Con _ ts)
  | con `elem` cs = (findIndex con cs, StructureType False (map convType ts))
  | otherwise = errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

findIndex :: (Pretty a, Eq a) => a -> [a] -> Integer
findIndex con cs = case List.elemIndex con cs of
  Just i -> fromIntegral i
  Nothing -> errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

globalStringPtr :: (MonadModuleBuilder m) => Text -> Name -> m C.Constant
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

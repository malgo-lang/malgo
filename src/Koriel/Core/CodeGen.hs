{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- LLVM IRの生成
module Koriel.Core.CodeGen
  ( codeGen,
  )
where

import Control.Lens (At (at), Lens', ifor, ifor_, lens, over, to, use, view, (<?=), (?=), (?~), (^.), (^?))
import Control.Monad.Fix (MonadFix)
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.List.Extra (headDef, maximum, mconcatMap)
import Data.String.Conversions
import Data.Traversable (for)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import qualified Koriel.Core.Op as Op
import Koriel.Core.Syntax
import Koriel.Core.Type as C
import Koriel.Id
import Koriel.Lens
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
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Global
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Linkage (Linkage (External, Internal))
import LLVM.AST.Operand (Operand (..))
import LLVM.AST.Type hiding
  ( double,
    void,
  )
import qualified LLVM.AST.Type as LT
import LLVM.AST.Typed (typeOf)
import LLVM.Context (withContext)
import LLVM.IRBuilder hiding (globalStringPtr, sizeof)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

instance Hashable Name

type PrimMap = HashMap Name Operand

-- 変数のHashMapとknown関数のHashMapを分割する
-- #7(https://github.com/takoeight0821/malgo/issues/7)のようなバグの早期検出が期待できる
data CodeGenEnv = CodeGenEnv
  { _codeGenUniqSupply :: UniqSupply,
    _valueMap :: HashMap (Id C.Type) Operand,
    _globalValueMap :: HashMap (Id C.Type) Operand,
    _funcMap :: HashMap (Id C.Type) Operand
  }

codeGenUniqSupply :: Lens' CodeGenEnv UniqSupply
codeGenUniqSupply = lens _codeGenUniqSupply (\c x -> c {_codeGenUniqSupply = x})

valueMap :: Lens' CodeGenEnv (HashMap (Id C.Type) Operand)
valueMap = lens _valueMap (\c x -> c {_valueMap = x})

globalValueMap :: Lens' CodeGenEnv (HashMap (Id C.Type) Operand)
globalValueMap = lens _globalValueMap (\c x -> c {_globalValueMap = x})

funcMap :: Lens' CodeGenEnv (HashMap (Id C.Type) Operand)
funcMap = lens _funcMap (\c x -> c {_funcMap = x})

instance HasUniqSupply CodeGenEnv UniqSupply where
  uniqSupply = codeGenUniqSupply

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

codeGen :: (MonadFix m, MonadFail m, MonadIO m) => FilePath -> FilePath -> UniqSupply -> ModuleName -> Program (Id C.Type) -> m ()
codeGen srcPath dstPath uniqSupply modName Program {..} = do
  llvmir <- runCodeGenT CodeGenEnv {_codeGenUniqSupply = uniqSupply, _valueMap = mempty, _globalValueMap = varEnv, _funcMap = funcEnv} do
    for_ _extFuncs \(name, typ) -> do
      let name' = LLVM.AST.mkName $ convertString name
      case typ of
        ps :-> r -> extern name' (map convType ps) (convType r)
        _ -> error "invalid type"
    traverse_ (uncurry genVar) _topVars
    traverse_ (\(f, (ps, body)) -> genFunc f ps body) _topFuncs
    genLoadModule modName $ initTopVars _topVars
  let llvmModule = defaultModule {LLVM.AST.moduleName = fromString srcPath, moduleSourceFileName = fromString srcPath, moduleDefinitions = llvmir}
  liftIO $ withContext $ \ctx -> writeFileBS dstPath =<< withModuleFromAST ctx llvmModule moduleLLVMAssembly
  where
    -- liftIO $ writeFileLText dstPath $ ppllvm llvmModule

    -- topVarsのOprMapを作成
    varEnv = mconcatMap ?? _topVars $ \(v, e) ->
      one (v, ConstantOperand $ C.GlobalReference (ptr $ convType $ C.typeOf e) (toName v))
    -- topFuncsのOprMapを作成
    funcEnv = mconcatMap ?? _topFuncs $ \(f, (ps, e)) ->
      one
        ( f,
          ConstantOperand $
            C.GlobalReference
              (ptr $ FunctionType (convType $ C.typeOf e) (map (convType . C.typeOf) ps) False)
              (toName f)
        )
    initTopVars [] = retVoid
    initTopVars ((name, expr) : xs) =
      view (globalValueMap . at name) >>= \case
        Nothing -> error $ show $ pPrint name <+> "is not found"
        Just name' -> genExp expr \eOpr -> do
          -- TODO[genExp does not return correctly-typed value]
          -- eOpr <- bitcast eOpr (convType (C.typeOf name))
          store name' 0 eOpr
          initTopVars xs

convType :: C.Type -> LT.Type
convType (ps :-> r) =
  ptr $
    StructureType False [ptr i8, ptr $ FunctionType (convType r) (ptr i8 : map convType ps) False]
convType Int32T = i32
convType Int64T = i64
convType FloatT = LT.float
convType DoubleT = LT.double
convType CharT = i8
convType StringT = ptr i8
convType BoolT = i8
convType (SumT cs) =
  let size = maximum $ sizeofCon <$> toList cs
   in ptr
        ( StructureType
            False
            [i8, if size == 0 then StructureType False [] else LT.VectorType size i8]
        )
convType (PtrT ty) = ptr $ convType ty
convType AnyT = ptr i8
convType VoidT = LT.void

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
        Just opr -> load opr 0 -- global variable is a pointer to the actual value
        Nothing -> findExtVar
    findExtVar =
      use (at $ toName x) >>= \case
        Just x -> load x 0
        Nothing -> internExtVar
    internExtVar = do
      emitDefn $
        GlobalDefinition
          globalVariableDefaults
            { LLVM.AST.Global.name = toName x,
              LLVM.AST.Global.type' = convType $ C.typeOf x,
              linkage = LLVM.AST.Linkage.External
            }
      let opr = ConstantOperand (C.GlobalReference (ptr $ convType $ C.typeOf x) (toName x))
      at (toName x) ?= opr
      load opr 0

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
mallocBytes bytesOpr maybeType = do
  gcMalloc <- findExt "GC_malloc" [i64] (ptr i8)
  ptrOpr <- call gcMalloc [(bytesOpr, [])]
  case maybeType of
    Just t -> bitcast ptrOpr t
    Nothing -> pure ptrOpr

mallocType :: (MonadCodeGen m, MonadIRBuilder m) => LT.Type -> m Operand
mallocType ty = mallocBytes (ConstantOperand $ sizeof ty) (Just $ ptr ty)

sizeof :: LT.Type -> C.Constant
sizeof ty = C.PtrToInt szPtr LT.i64
  where
    ptrType = LT.ptr ty
    nullPtr = C.IntToPtr (C.Int 32 0) ptrType
    szPtr = C.GetElementPtr True nullPtr [C.Int 32 1]

toName :: Id a -> LLVM.AST.Name
-- toName Id {_idName = "main", _idSort = Koriel.Id.External (ModuleName "Builtin")} = LLVM.AST.mkName "main"
toName id = LLVM.AST.mkName $ convertString $ idToText id

-- generate code for a toplevel variable definition
genVar :: MonadModuleBuilder m => Id C.Type -> Exp (Id C.Type) -> m Operand
genVar name expr = global (toName name) (convType $ C.typeOf expr) (C.Undef (convType $ C.typeOf expr))

genLoadModule :: MonadModuleBuilder m => ModuleName -> IRBuilderT m () -> m Operand
genLoadModule (ModuleName modName) m = function (LLVM.AST.mkName $ convertString $ "koriel_load_" <> modName) [] LT.void $ const m

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
  Exp (Id C.Type) ->
  m Operand
genFunc name params body
  | idIsExternal name || idIsNative name =
    function funcName llvmParams retty $ \args -> local (over valueMap (HashMap.fromList (zip params args) <>)) $ genExp body ret
  | otherwise =
    internalFunction funcName llvmParams retty $ \args -> local (over valueMap (HashMap.fromList (zip params args) <>)) $ genExp body ret
  where
    funcName = toName name
    llvmParams =
      map
        (\x -> (convType $ x ^. idMeta, ParameterName $ toShort $ encodeUtf8 $ idToText x))
        params
    retty = convType (C.typeOf body)

-- genUnpackでコード生成しつつラベルを返すため、CPSにしている
-- FIXME[genExp does not return correctly-typed value]: convType (C.typeOf e) /= LT.typeOf (genExp e)
--        継続に渡される値の型がptr i8だったときに正しくタグを取り出すため、bitcastする必要がある。
-- TODO: genExpが正しい型の値を継続に渡すように変更する
genExp ::
  ( MonadReader CodeGenEnv m,
    MonadState PrimMap m,
    MonadIRBuilder m,
    MonadModuleBuilder m,
    MonadFail m,
    MonadFix m,
    MonadIO m
  ) =>
  Exp (Id C.Type) ->
  (Operand -> m ()) ->
  m ()
genExp (Atom x) k = k =<< genAtom x
genExp (Call f xs) k = do
  fOpr <- genAtom f
  captureOpr <- gepAndLoad fOpr [int32 0, int32 0]
  funcOpr <- gepAndLoad fOpr [int32 0, int32 1]
  xsOprs <- traverse genAtom xs
  k =<< call funcOpr (map (,[]) $ captureOpr : xsOprs)
genExp (CallDirect f xs) k = do
  fOpr <- findFun f
  xsOprs <- traverse genAtom xs
  k =<< call fOpr (map (,[]) xsOprs)
genExp (RawCall name (ps :-> r) xs) k = do
  let primOpr =
        ConstantOperand $
          C.GlobalReference
            (ptr $ FunctionType (convType r) (map convType ps) False)
            (LLVM.AST.mkName $ convertString name)
  xsOprs <- traverse genAtom xs
  k =<< call primOpr (map (,[]) xsOprs)
genExp (RawCall _ t _) _ = error $ show $ pPrint t <> " is not fuction type"
genExp (BinOp o x y) k = k =<< join (genOp o <$> genAtom x <*> genAtom y)
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
genExp (Let xs e) k = do
  env <- foldMapM prepare xs
  env <- local (over valueMap (env <>)) $ mconcat <$> traverse genLocalDef xs
  local (over valueMap (env <>)) $ genExp e k
  where
    -- Generate a `malloc(sizeof(<closure type>))` call for a local function definition.
    prepare (LocalDef name (Fun ps body)) =
      one . (name,)
        <$> mallocType
          ( StructureType
              False
              [ ptr i8,
                ptr $ FunctionType (convType $ C.typeOf body) (ptr i8 : map (convType . C.typeOf) ps) False
              ]
          )
    prepare _ = pure mempty
genExp (Match e (Bind _ body :| _)) k | C.typeOf e == VoidT = genExp e $ \_ -> genExp body k
genExp (Match e (Bind x body :| _)) k = genExp e $ \eOpr -> do
  eOpr <- bitcast eOpr (convType $ C.typeOf e)
  local (over valueMap (at x ?~ eOpr)) (genExp body k)
genExp (Match e cs) k
  | C.typeOf e == VoidT = error "VoidT is not able to bind to variable."
  | otherwise = genExp e $ \eOpr -> mdo
    -- eOprの型がptr i8だったときに正しくタグを取り出すため、bitcastする
    -- TODO[genExp does not return correctly-typed value]
    -- genExpが正しい型の値を継続に渡すように変更する
    eOpr' <- bitcast eOpr (convType $ C.typeOf e)
    br switchBlock
    -- 各ケースのコードとラベルを生成する
    -- switch用のタグがある場合は Right (タグ, ラベル) を、ない場合は Left タグ を返す
    (defaults, labels) <- partitionEithers . toList <$> traverse (genCase eOpr' (maybeToMonoid $ e ^? to C.typeOf . _SumT) k) cs
    -- defaultsの先頭を取り出し、switchのデフォルトケースとする
    -- defaultsが空の場合、デフォルトケースはunreachableにジャンプする
    defaultLabel <- headDef (block >>= \l -> unreachable >> pure l) $ map pure defaults
    switchBlock <- block
    tagOpr <- case C.typeOf e of
      SumT _ -> gepAndLoad eOpr' [int32 0, int32 0]
      _ -> pure eOpr'
    switch tagOpr defaultLabel labels
genExp (Cast ty x) k = do
  xOpr <- genAtom x
  k =<< bitcast xOpr (convType ty)
genExp (Error _) _ = unreachable

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
  Bind x e -> do
    label <- block
    void $ local (over valueMap $ at x ?~ scrutinee) $ genExp e k
    pure $ Left label
  Switch u e -> do
    ConstantOperand u' <- genAtom $ Unboxed u
    label <- block
    genExp e k
    pure $ Right (u', label)
  Unpack con vs e -> do
    label <- block
    let (tag, conType) = genCon cs con
    addr <- bitcast scrutinee (ptr $ StructureType False [i8, conType])
    payloadAddr <- gep addr [int32 0, int32 1]
    -- WRONG: payloadAddr <- (bitcast ?? ptr conType) =<< gep scrutinee [int32 0, int32 1]
    env <-
      HashMap.fromList <$> ifor vs \i v ->
        (v,) <$> gepAndLoad payloadAddr [int32 0, int32 $ fromIntegral i]
    void $ local (over valueMap (env <>)) $ genExp e k
    pure $ Right (C.Int 8 tag, label)

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
genLocalDef (LocalDef funName (Fun ps e)) = do
  -- クロージャの元になる関数を生成する
  name <- toName <$> newInternalId (idToText funName <> "_closure") ()
  func <- internalFunction name (map (,NoParameterName) psTypes) retType $ \case
    [] -> error "The length of internal function parameters must be 1 or more"
    (rawCapture : ps') -> do
      -- キャプチャした変数が詰まっている構造体を展開する
      capture <- bitcast rawCapture (ptr capType)
      env <-
        HashMap.fromList <$> ifor fvs \i fv ->
          (fv,) <$> gepAndLoad capture [int32 0, int32 $ fromIntegral i]
      local (over valueMap ((env <> HashMap.fromList (zip ps ps')) <>)) $ genExp e ret
  -- キャプチャされる変数を構造体に詰める
  capture <- mallocType capType
  ifor_ fvs $ \i fv -> do
    fvOpr <- findVar fv
    gepAndStore capture [int32 0, int32 $ fromIntegral i] fvOpr
  closAddr <- findVar funName
  gepAndStore closAddr [int32 0, int32 0] =<< bitcast capture (ptr i8)
  gepAndStore closAddr [int32 0, int32 1] func
  pure $ one (funName, closAddr)
  where
    fvs = toList $ freevars (Fun ps e)
    -- キャプチャされる変数を詰める構造体の型
    capType = StructureType False (map (convType . C.typeOf) fvs)
    psTypes = ptr i8 : map (convType . C.typeOf) ps
    retType = convType $ C.typeOf e
genLocalDef (LocalDef name@(C.typeOf -> SumT cs) (Pack _ con@(Con _ ts) xs)) = do
  addr <- mallocType (StructureType False [i8, StructureType False $ map convType ts])
  -- タグの書き込み
  gepAndStore addr [int32 0, int32 0] (int8 $ findIndex con cs)
  -- 引数の書き込み
  ifor_ xs $ \i x ->
    gepAndStore addr [int32 0, int32 1, int32 $ fromIntegral i] =<< genAtom x
  -- nameの型にキャスト
  one . (name,) <$> bitcast addr (convType $ SumT cs)
genLocalDef (LocalDef (C.typeOf -> t) Pack {}) = error $ show t <> " must be SumT"

genCon :: [Con] -> Con -> (Integer, LT.Type)
genCon cs con@(Con _ ts)
  | con `elem` cs = (findIndex con cs, StructureType False (map convType ts))
  | otherwise = errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

findIndex :: (Pretty a, Eq a) => a -> [a] -> Integer
findIndex con cs = case List.elemIndex con cs of
  Just i -> fromIntegral i
  Nothing -> errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

-- sizeof :: LT.Type -> Operand
-- sizeof ty = ConstantOperand $ C.PtrToInt szPtr LT.i64
--   where
--     ptrType = LT.ptr ty
--     nullPtr = C.IntToPtr (C.Int 32 0) ptrType
--     szPtr = C.GetElementPtr True nullPtr [C.Int 32 1]

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
  pure $ C.GetElementPtr True (C.GlobalReference (ptr ty) nm) [C.Int 32 0, C.Int 32 0]

gepAndLoad ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  Operand ->
  [Operand] ->
  m Operand
gepAndLoad opr addrs = do
  addr <- gep opr addrs
  load addr 0

gepAndStore ::
  (MonadIRBuilder m, MonadModuleBuilder m) =>
  Operand ->
  [Operand] ->
  Operand ->
  m ()
gepAndStore opr addrs val = do
  addr <- gep opr addrs
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
      funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label

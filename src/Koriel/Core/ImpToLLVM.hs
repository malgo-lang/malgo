{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Koriel.Core.ImpToLLVM where

import Control.Lens (At (at), ifor, ifor_, over, use, view, (<?=), (?=), (^.))
import Control.Lens.TH (makeLenses)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (Foldable (maximum))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.List.Extra (mconcatMap)
import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Traversable (for)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Koriel.Core.Imp
import qualified Koriel.Core.Op as Op
import Koriel.Core.Type (Con (Con), Type (..))
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import LLVM (moduleLLVMAssembly, withModuleFromAST)
import LLVM.AST (Definition (GlobalDefinition), Module (moduleDefinitions, moduleName, moduleSourceFileName), Name, Operand (ConstantOperand, LocalReference), Parameter (Parameter), Type (IntegerType), UnnamedAddr (GlobalAddr), defaultModule, functionDefaults, globalVariableDefaults, mkName)
import LLVM.AST.Constant (Constant)
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Global (Global (basicBlocks, initializer, isConstant, linkage, name, parameters, returnType, type', unnamedAddr))
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Linkage as Linkage
import LLVM.AST.Type (Type (FunctionType, StructureType), i32, i64, i8, ptr)
import qualified LLVM.AST.Type as LT
import qualified LLVM.AST.Typed
import LLVM.Context (withContext)
import LLVM.IRBuilder hiding (globalStringPtr, sizeof)

data CodeGenEnv m = CodeGenEnv
  { _codeGenUniqSupply :: UniqSupply,
    _localValueMap :: HashMap (Id C.Type) Operand,
    _globalValueMap :: HashMap (Id C.Type) Operand,
    _functionMap :: HashMap (Id C.Type) Operand,
    _returnOperator :: Operand -> m (),
    _breakOperator :: Operand -> m ()
  }

makeLenses ''CodeGenEnv

instance HasUniqSupply (CodeGenEnv m) where
  uniqSupply = codeGenUniqSupply

newtype CodeGenT m a = CodeGenT
  { runCodeGenT ::
      Lazy.StateT
        PrimMap
        ( ReaderT
            (CodeGenEnv (IRBuilderT (CodeGenT m))) -- stores 'CodeGenEnv n' in the 'ReaderT', where 'n = IRBuilderT (CodeGenT m)'.
            m
        )
        a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState PrimMap, MonadReader (CodeGenEnv (IRBuilderT (CodeGenT m))), MonadModuleBuilder)

instance MonadTrans CodeGenT where
  lift m = CodeGenT (lift $ lift m)

mkCodeGenEnv :: MonadIRBuilder m => UniqSupply -> HashMap (Id C.Type) Operand -> HashMap (Id C.Type) Operand -> CodeGenEnv m
mkCodeGenEnv _codeGenUniqSupply _globalValueMap _functionMap =
  let _localValueMap = mempty
      _returnOperator = ret
      _breakOperator = const pass
   in CodeGenEnv {..}

type PrimMap = HashMap Name Operand

-- runCodeGenT :: Monad m => CodeGenEnv -> Lazy.StateT PrimMap (ReaderT CodeGenEnv (ModuleBuilderT m)) a -> m [Definition]
execCodeGenT :: Monad m => CodeGenEnv (IRBuilderT (CodeGenT m)) -> CodeGenT m a -> m a
execCodeGenT env (CodeGenT m) = runReaderT (Lazy.evalStateT m mempty) env

codeGen :: MonadIO m => FilePath -> FilePath -> UniqSupply -> ModuleName -> Program (Id C.Type) -> m ()
codeGen srcPath dstPath uniqSupply modName Program {..} = do
  llvmir <- execModuleBuilderT emptyModuleBuilder $ execCodeGenT (mkCodeGenEnv uniqSupply globalVarEnv funcEnv) do
    for_ _extFuncs \(name, typ) -> do
      let name' = LLVM.AST.mkName $ convertString name
      case typ of
        ps :-> r -> extern name' (map convType ps) (convType r)
        _ -> error "invalid type"
    traverse_ (uncurry genGlobalVar) _topVars
    traverse_ (\(f, (ps, body)) -> genFunc f ps body) _topFuncs
    genLoadModule modName $ initTopVars _topVars
  let llvmModule = defaultModule {LLVM.AST.moduleName = fromString srcPath, moduleSourceFileName = fromString srcPath, moduleDefinitions = llvmir}
  liftIO $ withContext $ \ctx -> writeFileBS dstPath =<< withModuleFromAST ctx llvmModule moduleLLVMAssembly
  where
    -- Construct OprMap for topVars
    globalVarEnv = mconcatMap ?? _topVars $ \(v, e) ->
      one (v, ConstantOperand $ Constant.GlobalReference (ptr $ convType $ C.typeOf e) (toName v))
    -- Construct OprMap for topFuncs
    funcEnv = mconcatMap ?? _topFuncs $ \(f, (ps, e)) ->
      one
        ( f,
          ConstantOperand $
            Constant.GlobalReference
              (ptr $ FunctionType (convType $ C.typeOf e) (map (convType . C.typeOf) ps) False)
              (toName f)
        )
    initTopVars [] = retVoid
    initTopVars ((name, block) : xs) =
      view (globalValueMap . at name) >>= \case
        Nothing -> errorDoc $ pPrint name <+> "is not found"
        Just name' -> do
          local (over returnOperator (const (store name' 0))) $ genBlock block
          initTopVars xs

toName :: Id a -> LLVM.AST.Name
toName Id {_idName = "main", _idSort = Koriel.Id.External (ModuleName "Builtin")} = LLVM.AST.mkName "main"
toName id = LLVM.AST.mkName $ convertString $ idToText id

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

sizeofCon :: Con -> Word32
sizeofCon (Con _ ts) = sum $ map sizeofType ts

sizeofType :: C.Type -> Word32
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

-- | Generate a (LLVM-level) global variable declaration for a (Malgo-level) toplevel variable definition.
-- It doesn't initialize the variable.
genGlobalVar :: MonadModuleBuilder m => Id C.Type -> Block (Id C.Type) -> m Operand
genGlobalVar name body = global (toName name) typ (Constant.Undef typ)
  where
    typ = convType $ C.typeOf body

-- | Generate a function that initializes all toplevel variables
genLoadModule :: MonadModuleBuilder m => ModuleName -> IRBuilderT m () -> m Operand
genLoadModule (ModuleName modName) m = function (mkName $ convertString $ "koriel_load_" <> modName) [] LT.void $ const m

-- | Generate a function corresponding a 'known' function in Malgo.
-- Known functions does not have free variables.
genFunc :: (MonadReader (CodeGenEnv (IRBuilderT m)) m, MonadModuleBuilder m, MonadIO m, MonadState PrimMap m) => Id C.Type -> [Id C.Type] -> Block (Id C.Type) -> m Operand
genFunc name params body =
  func funcName llvmParams (convType $ C.typeOf body) $ \args -> local (over localValueMap (HashMap.fromList (zip params args) <>)) $ genBlock body
  where
    funcName = toName name
    llvmParams = map (\x -> (convType $ x ^. idMeta, ParameterName $ toShort $ encodeUtf8 $ idToText x)) params
    func = if idIsExternal name then function else internalFunction

genBlock :: (MonadReader (CodeGenEnv f) f, MonadIO f, MonadModuleBuilder f, MonadIRBuilder f, MonadState PrimMap f) => Block (Id C.Type) -> f ()
genBlock Block {_statements} = genStmts _statements

genStmts :: (MonadReader (CodeGenEnv f) f, MonadIO f, MonadModuleBuilder f, MonadIRBuilder f, MonadState PrimMap f) => [Stmt (Id C.Type)] -> f ()
genStmts [] = pass
genStmts (Eval x e : ss) = do
  e' <- genExp (C.typeOf x) e
  local (over localValueMap (HashMap.insert x e')) $ genStmts ss
genStmts (Let ds : ss) = do
  env <- foldMapM prepare ds
  env <- local (over localValueMap (env <>)) $ mconcat <$> traverse genLocalDef ds
  local (over localValueMap (env <>)) $ genStmts ss
  where
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
genStmts (Match result scrutinee branches : ss) = do
  _
genStmts (Break x : _) = do
  x' <- findVar x
  breakOp <- view breakOperator
  breakOp x'
genStmts (Return x : _) = do
  x' <- findVar x
  retOp <- view returnOperator
  retOp x'

genLocalDef :: (MonadModuleBuilder m, MonadIO m, MonadState PrimMap m, MonadIRBuilder m, MonadReader (CodeGenEnv m) m) => LocalDef (Id C.Type) -> m (HashMap (Id C.Type) Operand)
genLocalDef (LocalDef funName (Fun ps b)) = do
  -- Generate the base function of the closure 'Fun ps b'
  func <- genBaseFunction
  -- Pack free variables to the capture environment
  capture <- mallocType capType
  ifor_ fvs $ \i fv -> do
    fvOpr <- findVar fv
    gepAndStore capture [int32 0, int32 $ fromIntegral i] fvOpr
  closAddr <- findVar funName
  gepAndStore closAddr [int32 0, int32 0] =<< bitcast capture (ptr i8)
  gepAndStore closAddr [int32 0, int32 1] func
  pure $ one (funName, closAddr)
  where
    fvs = toList $ freevars $ Fun ps b
    capType = StructureType False (map (convType . C.typeOf) fvs)
    psTypes = ptr i8 : map (convType . C.typeOf) ps
    retType = convType $ C.typeOf b
    genBaseFunction = do
      name <- toName <$> newInternalId (idToText funName <> "_closure") ()
      env <- ask
      internalFunction name (map (,NoParameterName) psTypes) retType $ \args -> do
        state <- liftIRState get
        runIn
          (env {_breakOperator = const pass, _returnOperator = ret})
          state
          case args of
            [] -> error "The length of internal function parameters must be 1 or more"
            (rawCapture : ps') -> do
              -- Extract the caputure environment
              capture <- bitcast rawCapture (ptr capType)
              env <-
                HashMap.fromList <$> ifor fvs \i fv ->
                  (fv,) <$> gepAndLoad capture [int32 0, int32 $ fromIntegral i]
              local (over localValueMap ((env <> HashMap.fromList (zip ps ps')) <>)) $
                local (over returnOperator (const ret)) $ genBlock b
    runIn :: Monad m => CodeGenEnv (IRBuilderT (CodeGenT m)) -> IRBuilderState -> IRBuilderT (CodeGenT m) a -> IRBuilderT m a
    runIn env s (IRBuilderT m) = do
      (r, s) <- lift $ execCodeGenT env $ runStateT m s
      liftIRState (modify (const s))
      pure r
genLocalDef (LocalDef name@(C.typeOf -> SumT cs) (Pack _ con@(Con _ ts) xs)) = do
  addr <- mallocType (StructureType False [i8, StructureType False $ map convType ts])
  gepAndStore addr [int32 0, int32 0] (int8 $ findIndex con cs)
  ifor_ xs $ \i x -> gepAndStore addr [int32 0, int32 1, int32 $ fromIntegral i] =<< genAtom x
  one . (name,) <$> bitcast addr (convType $ SumT cs)
genLocalDef (LocalDef (C.typeOf -> t) Pack {}) = error $ show t <> " must be SumT"

findIndex :: (Eq a, Pretty a) => a -> [a] -> Integer
findIndex con cs = case List.elemIndex con cs of
  Just i -> fromIntegral i
  Nothing -> errorDoc $ pPrint con <+> "is not in" <+> pPrint cs

genExp :: (MonadReader (CodeGenEnv m) m, MonadIO m, MonadModuleBuilder m, MonadIRBuilder m, MonadState PrimMap m) => C.Type -> Exp (Id C.Type) -> m Operand
genExp t e
  | t == C.typeOf e = genExp' e
  | otherwise = error "not implemented: insert cast"

genExp' :: (MonadReader (CodeGenEnv m) m, MonadIO m, MonadModuleBuilder m, MonadIRBuilder m, MonadState PrimMap m) => Exp (Id C.Type) -> m Operand
genExp' (Atom x) = genAtom x
genExp' (Call f xs) = do
  fOpr <- genAtom f
  captureOpr <- gepAndLoad fOpr [int32 0, int32 0]
  funcOpr <- gepAndLoad fOpr [int32 0, int32 1]
  xsOprs <- traverse genAtom xs
  call funcOpr (map (,[]) $ captureOpr : xsOprs)
genExp' (CallDirect f xs) = do
  fOpr <- findFun f
  xsOprs <- traverse genAtom xs
  call fOpr (map (,[]) xsOprs)
genExp' (RawCall name (ps :-> r) xs) = do
  let primOpr =
        ConstantOperand $
          Constant.GlobalReference
            (ptr $ FunctionType (convType r) (map convType ps) False)
            (mkName $ convertString name)
  xsOprs <- traverse genAtom xs
  call primOpr (map (,[]) xsOprs)
genExp' (BinOp o x y) = join (genOp o <$> genAtom x <*> genAtom y)
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
    genOp Op.And =
      LLVM.IRBuilder.and
    genOp Op.Or =
      LLVM.IRBuilder.or
    i1ToBool i1opr =
      zext i1opr i8

genAtom :: (MonadReader (CodeGenEnv m) m, MonadIO m, MonadModuleBuilder m, MonadIRBuilder m, MonadState PrimMap m) => Atom (Id C.Type) -> m Operand
genAtom (Var x) = findVar x
genAtom (Unboxed u) = case u of
  Int32 x -> pure $ int32 x
  Int64 x -> pure $ int64 x
  Float x -> pure $ ConstantOperand $ Constant.BitCast (Constant.Int 32 $ toInteger $ castFloatToWord32 x) LT.float
  Double x -> pure $ ConstantOperand $ Constant.BitCast (Constant.Int 64 $ toInteger $ castDoubleToWord64 x) LT.double
  Char x -> pure $ int8 $ toInteger $ ord x
  String x -> do
    i <- getUniq
    ConstantOperand <$> globalStringPtr x (mkName $ "str" <> show i)
  Bool True -> pure $ int8 1
  Bool False -> pure $ int8 0

findVar :: (MonadState PrimMap m, MonadIRBuilder m, MonadModuleBuilder m, MonadReader (CodeGenEnv m) m) => Id C.Type -> m Operand
findVar x = findLocalVar
  where
    findLocalVar =
      view (localValueMap . at x) >>= \case
        Just opr -> pure opr
        Nothing -> findGlobalVar
    findGlobalVar =
      view (globalValueMap . at x) >>= \case
        Just opr -> load opr 0 -- global variable is a pointer to the actual value
        Nothing -> findExtVar
    findExtVar =
      use (at $ toName x) >>= \case
        Just opr -> load opr 0
        Nothing -> internExtVar
    internExtVar = do
      emitDefn $
        GlobalDefinition
          globalVariableDefaults
            { name = toName x,
              type' = convType $ C.typeOf x,
              linkage = Linkage.External
            }
      let opr = ConstantOperand (Constant.GlobalReference (ptr $ convType $ C.typeOf x) (toName x))
      at (toName x) ?= opr
      load opr 0

findFun :: (MonadReader (CodeGenEnv m) m, MonadState PrimMap m, MonadModuleBuilder m) => Id C.Type -> m Operand
findFun x =
  view (functionMap . at x) >>= \case
    Just opr -> pure opr
    Nothing ->
      case C.typeOf x of
        ps :-> r -> findExt (toName x) (map convType ps) (convType r)
        _ -> error $ show $ pPrint x <> " is not found"

-- | Intern a external function or return the interned external function
findExt :: (MonadState PrimMap m, MonadModuleBuilder m) => Name -> [LT.Type] -> LT.Type -> m Operand
findExt x ps r =
  use (at x) >>= \case
    Just x -> pure x
    Nothing -> do
      ext <- extern x ps r
      at x <?= ext

-- | 'function', but linkage = Internal
internalFunction ::
  MonadModuleBuilder m =>
  -- | Function name
  Name ->
  -- | Parameter types and name hints
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
            { name = label,
              linkage = Linkage.Internal,
              parameters = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False),
              returnType = retty,
              basicBlocks = blocks
            }
      funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ Constant.GlobalReference funty label

globalStringPtr :: MonadModuleBuilder m => Text -> Name -> m Constant.Constant
globalStringPtr str nm = do
  let utf8Vals = map toInteger $ BL.unpack $ convertString str
      llvmVals = map (Constant.Int 8) (utf8Vals ++ [0])
      char = IntegerType 8
      charArray = Constant.Array char llvmVals
  ty <-
    LLVM.AST.Typed.typeOf charArray >>= \case
      Left err -> error $ show err
      Right ty -> pure ty
  emitDefn $
    GlobalDefinition
      globalVariableDefaults
        { name = nm,
          LLVM.AST.Global.type' = ty,
          linkage = Linkage.External,
          isConstant = True,
          initializer = Just charArray,
          unnamedAddr = Just GlobalAddr
        }
  pure $ Constant.GetElementPtr True (Constant.GlobalReference (ptr ty) nm) [Constant.Int 32 0, Constant.Int 32 0]

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

mallocType :: (MonadState PrimMap m, MonadModuleBuilder m, MonadIRBuilder m) => LT.Type -> m Operand
mallocType ty = mallocBytes (ConstantOperand $ sizeof ty) (Just $ ptr ty)

mallocBytes :: (MonadState PrimMap m, MonadModuleBuilder m, MonadIRBuilder m) => Operand -> Maybe LT.Type -> m Operand
mallocBytes bytesOpr maybeType = do
  gcMalloc <- findExt "GC_malloc" [i64] (ptr i8)
  ptrOpr <- call gcMalloc [(bytesOpr, [])]
  case maybeType of
    Just t -> bitcast ptrOpr t
    Nothing -> pure ptrOpr

sizeof :: LT.Type -> Constant
sizeof ty = Constant.PtrToInt szPtr LT.i64
  where
    ptrType = LT.ptr ty
    nullPtr = Constant.IntToPtr (Constant.Int 32 0) ptrType
    szPtr = Constant.GetElementPtr True nullPtr [Constant.Int 32 1]

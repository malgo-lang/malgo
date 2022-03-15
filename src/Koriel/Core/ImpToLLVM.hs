{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Koriel.Core.ImpToLLVM where

import Control.Lens (At (at), over, use, view, (?=), (^.))
import Control.Lens.TH (makeLenses)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (Foldable (maximum))
import qualified Data.HashMap.Strict as HashMap
import Data.List.Extra (mconcatMap)
import Data.String.Conversions (ConvertibleStrings (convertString))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Koriel.Core.Imp
import Koriel.Core.Type (Con (Con), Type (..))
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import LLVM (moduleLLVMAssembly, withModuleFromAST)
import LLVM.AST (Definition (GlobalDefinition), Module (moduleDefinitions, moduleName, moduleSourceFileName), Name, Operand (ConstantOperand), Type (IntegerType), UnnamedAddr (GlobalAddr), defaultModule, globalVariableDefaults, mkName)
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Global (Global (initializer, isConstant, linkage, name, type', unnamedAddr))
import qualified LLVM.AST.Linkage as Linkage
import LLVM.AST.Type (Type (FunctionType, StructureType), i32, i64, i8, ptr)
import qualified LLVM.AST.Type as LT
import qualified LLVM.AST.Typed
import LLVM.Context (withContext)
import LLVM.IRBuilder hiding (globalStringPtr)

data CodeGenEnv = CodeGenEnv
  { _codeGenUniqSupply :: UniqSupply,
    _localValueMap :: HashMap (Id C.Type) Operand,
    _globalValueMap :: HashMap (Id C.Type) Operand,
    _functionMap :: HashMap (Id C.Type) Operand
  }

makeLenses ''CodeGenEnv

instance HasUniqSupply CodeGenEnv where
  uniqSupply = codeGenUniqSupply

mkCodeGenEnv :: UniqSupply -> HashMap (Id C.Type) Operand -> HashMap (Id C.Type) Operand -> CodeGenEnv
mkCodeGenEnv _codeGenUniqSupply _globalValueMap _functionMap =
  let _localValueMap = mempty in CodeGenEnv {..}

type PrimMap = HashMap Name Operand

type MonadCodeGen m =
  ( MonadModuleBuilder m,
    MonadReader CodeGenEnv m,
    MonadState PrimMap m
  ) ::
    Constraint

runCodeGenT :: Monad m => CodeGenEnv -> Lazy.StateT PrimMap (ReaderT CodeGenEnv (ModuleBuilderT m)) a -> m [Definition]
runCodeGenT env m = execModuleBuilderT emptyModuleBuilder $ runReaderT (Lazy.evalStateT m mempty) env

codeGen :: MonadIO m => FilePath -> FilePath -> UniqSupply -> p -> Program (Id C.Type) -> m ()
codeGen srcPath dstPath uniqSupply modName Program {..} = do
  llvmir <- runCodeGenT (mkCodeGenEnv uniqSupply globalVarEnv funcEnv) do
    for_ _extFuncs \(name, typ) -> do
      let name' = LLVM.AST.mkName $ convertString name
      case typ of
        ps :-> r -> extern name' (map convType ps) (convType r)
        _ -> error "invalid type"
    traverse_ (uncurry genGlobalVar) _topVars
    traverse_ (\(f, (ps, body)) -> genFunc f ps body) _topFuncs
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

-- | Generate a function corresponding a 'known' function in Malgo.
-- Known functions does not have free variables.
genFunc :: (MonadModuleBuilder m, MonadReader CodeGenEnv m, MonadState PrimMap m, MonadIO m) => Id C.Type -> [Id C.Type] -> Block (Id C.Type) -> m Operand
genFunc name params body =
  func funcName llvmParams (convType $ C.typeOf body) $ \args -> local (over localValueMap (HashMap.fromList (zip params args) <>)) $ genBlock body
  where
    funcName = toName name
    llvmParams = map (\x -> (convType $ x ^. idMeta, ParameterName $ toShort $ encodeUtf8 $ idToText x)) params
    func = if idIsExternal name then function else internalFunction

genBlock Block {_statements} = genStmts _statements

genStmts [] = pure ()
genStmts (Eval x e : ss) = do
  e' <- genExp (C.typeOf x) e
  local (over localValueMap (HashMap.insert x e')) $ genStmts ss

genExp t e
  | t == C.typeOf e = genExp' e
  | otherwise = error "not implemented: insert cast"

genExp' (Atom x) = genAtom x

genAtom :: (MonadReader CodeGenEnv m, MonadIO m, MonadModuleBuilder m, MonadIRBuilder m, MonadState PrimMap m) => Atom (Id C.Type) -> m Operand
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

findVar :: (MonadState PrimMap m, MonadIRBuilder m, MonadModuleBuilder m, MonadReader CodeGenEnv m) => Id C.Type -> m Operand
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
internalFunction = _

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

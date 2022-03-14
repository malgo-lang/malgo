{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.ImpToLLVM where

import Control.Lens.TH (makeLenses)
import qualified Control.Monad.State.Lazy as Lazy
import Data.String.Conversions (ConvertibleStrings (convertString))
import Koriel.Core.Imp
import Koriel.Core.Type (Type (..), Con (Con))
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import LLVM (moduleLLVMAssembly, withModuleFromAST)
import LLVM.AST (Definition, Module (moduleDefinitions, moduleName, moduleSourceFileName), Operand, defaultModule, mkName)
import LLVM.AST.Name (Name)
import qualified LLVM.AST.Type as LT
import LLVM.Context (withContext)
import LLVM.IRBuilder
import LLVM.AST.Type (ptr, Type (StructureType, FunctionType), i8, i32, i64)
import Data.Foldable (Foldable(maximum))

data CodeGenEnv = CodeGenEnv
  { _codeGenUniqSupply :: UniqSupply,
    _localValueMap :: HashMap (Id C.Type) Operand,
    _globalValueMap :: HashMap (Id C.Type) Operand,
    _functionMap :: HashMap (Id C.Type) Operand
  }

makeLenses ''CodeGenEnv

instance HasUniqSupply CodeGenEnv where
  uniqSupply = codeGenUniqSupply

type PrimMap = HashMap Name Operand

type MonadCodeGen m =
  ( MonadModuleBuilder m,
    MonadReader CodeGenEnv m,
    MonadState PrimMap m
  ) ::
    Constraint

runCodeGenT :: Monad m => CodeGenEnv -> Lazy.StateT PrimMap (ReaderT CodeGenEnv (ModuleBuilderT m)) a -> m [Definition]
runCodeGenT env m = execModuleBuilderT emptyModuleBuilder $ runReaderT (Lazy.evalStateT m mempty) env

codeGen srcPath dstPath uniqSupply modName Program {..} = do
  llvmir <- runCodeGenT CodeGenEnv {_codeGenUniqSupply = uniqSupply, _localValueMap = mempty, _globalValueMap = varEnv, _functionMap = funcEnv} do
    for_ _extFuncs \(name, typ) -> do
      let name' = LLVM.AST.mkName $ convertString name
      case typ of
        ps :-> r -> extern name' (map convType ps) (convType r)
        _ -> error "invalid type"
  let llvmModule = defaultModule {LLVM.AST.moduleName = fromString srcPath, moduleSourceFileName = fromString srcPath, moduleDefinitions = llvmir}
  liftIO $ withContext $ \ctx -> writeFileBS dstPath =<< withModuleFromAST ctx llvmModule moduleLLVMAssembly
  where
    -- Construct OprMap for topVars
    varEnv = _
    -- Construct OprMap for topFuncs
    funcEnv = _

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

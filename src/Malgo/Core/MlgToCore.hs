module Malgo.Core.MlgToCore (mlgToCore) where

import Control.Lens (At (at), Lens', lens, use, (%=), (.=), (?=), (^.), _Cons)
import Koriel.Id
import Koriel.MonadUniq (HasUniqSupply)
import Malgo.Core.Syntax
import Malgo.Infer.TcEnv (TcEnv)
import qualified Malgo.Infer.TcEnv as TcEnv
import Malgo.Prelude
import qualified Malgo.Syntax as S
import Malgo.Syntax.Extension (Malgo, MalgoPhase (Refine), RnId)
import qualified Malgo.TypeRep as S

data DsEnv = DsEnv {_buildingModule :: Module, _tcEnv :: TcEnv}

buildingModule :: Lens' DsEnv Module
buildingModule = lens _buildingModule \e x -> e {_buildingModule = x}

tcEnv :: Lens' DsEnv TcEnv
tcEnv = lens _tcEnv \e x -> e {_tcEnv = x}

mlgToCore :: (MonadIO m, MonadReader env m, HasUniqSupply env, MonadFail m) => TcEnv -> S.Module (Malgo 'Refine) -> m Module
mlgToCore tcEnv (S.Module _ S.BindGroup {_scDefs}) = evaluatingStateT
  DsEnv
    { _buildingModule = Module {_variableDefinitions = [], _externalDefinitions = [], _typeDefinitions = []},
      _tcEnv = tcEnv
    }
  do
    traverse_ dsScDefs _scDefs
    use buildingModule

dsNewVarName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => RnId -> m Name
dsNewVarName old = do
  Just scheme <- use (tcEnv . TcEnv.varEnv . at old)
  scheme <- dsScheme scheme
  newIdOnName scheme old

dsNewTyVarName _ = undefined

dsScheme (S.Forall [] ty) = dsType ty
dsScheme (S.Forall (p : ps) ty) = TyForall <$> dsNewTyVarName p <*> dsScheme (S.Forall ps ty)

dsType (S.TyPrim p) = pure $ TyPrim p

dsScDefs ds = traverse dsScDef ds

dsScDef (_, name, expr) = do
  name <- dsNewVarName name
  expr <- dsExp expr (name ^. idMeta)
  buildingModule . variableDefinitions %= ((name, expr) :)

dsExp (S.Unboxed _ (S.Int32 x)) (TyPrim S.Int32T) = pure $ Unboxed $ Int32 x

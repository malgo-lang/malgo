{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Malgo.Core.MlgToCore (mlgToCore) where

import Control.Lens (At (at), Lens', lens, use, (%=), (^.), (<?=))
import Koriel.Id
import Koriel.MonadUniq (HasUniqSupply)
import Malgo.Core.Syntax
import Malgo.Infer.TcEnv (TcEnv)
import qualified Malgo.Infer.TcEnv as TcEnv
import Malgo.Prelude
import qualified Malgo.Syntax as S
import Malgo.Syntax.Extension (Malgo, MalgoPhase (Refine), RnId)
import qualified Malgo.TypeRep as T

data DsEnv = DsEnv
  { _buildingModule :: Module, -- ^ 脱糖の結果が詰め込まれるModule
    _interned :: HashMap RnId Name, -- ^ インターン済みのシンボル
    _tcEnv :: TcEnv
  }

buildingModule :: Lens' DsEnv Module
buildingModule = lens _buildingModule \e x -> e {_buildingModule = x}

interned :: Lens' DsEnv (HashMap RnId Name)
interned = lens _interned \e x -> e {_interned = x}

tcEnv :: Lens' DsEnv TcEnv
tcEnv = lens _tcEnv \e x -> e {_tcEnv = x}

mlgToCore :: (MonadIO m, MonadReader env m, HasUniqSupply env, MonadFail m) => TcEnv -> S.Module (Malgo 'Refine) -> m Module
mlgToCore tcEnv (S.Module _ S.BindGroup {_scDefs, _dataDefs}) = evaluatingStateT
  DsEnv
    { _buildingModule = Module {_variableDefinitions = [], _externalDefinitions = [], _typeDefinitions = []},
      _interned = mempty,
      _tcEnv = tcEnv
    }
  do
    -- dsTypeDef =<< use (tcEnv . TcEnv.typeEnv)
    traverse_ dsScDefs _scDefs
    use buildingModule

-- | インターンしたシンボル（Name）を返す
dsVarName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => RnId -> m Name
dsVarName old = do
  use (interned . at old) >>= \case
    Just name -> pure name
    Nothing -> do
      Just scheme <- use (tcEnv . TcEnv.varEnv . at old)
      scheme <- dsScheme scheme
      name <- newIdOnName scheme old
      interned . at old <?= name

dsNewTyVarName :: Monad m => Id T.Type -> m Name
dsNewTyVarName _ = undefined

dsScheme :: Monad f => T.Scheme T.Type -> f Type
dsScheme (T.Forall [] ty) = dsType ty
dsScheme (T.Forall (p : ps) ty) = TyForall <$> dsNewTyVarName p <*> dsScheme (T.Forall ps ty)

dsType :: Applicative f => T.Type -> f Type
dsType (T.TyPrim p) = pure $ TyPrim p

dsScDefs :: (MonadState DsEnv f, MonadIO f, HasUniqSupply env, MonadReader env f, MonadFail f) => [S.ScDef (Malgo 'Refine)] -> f ()
dsScDefs = traverse_ dsScDef

dsScDef :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => S.ScDef (Malgo 'Refine) -> m ()
dsScDef (_, name, expr) = do
  name <- dsVarName name
  expr <- dsExp expr (name ^. idMeta)
  buildingModule . variableDefinitions %= ((name, expr) :)

dsExp :: Applicative f => S.Exp (Malgo 'Refine) -> Type -> f Exp
dsExp (S.Unboxed _ (S.Int32 x)) (TyPrim T.Int32T) = pure $ Unboxed $ Int32 x

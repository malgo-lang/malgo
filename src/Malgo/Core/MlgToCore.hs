module Malgo.Core.MlgToCore (mlgToCore) where

import Control.Lens (Lens', at, ifor, ifor_, lens, to, use, (?=), (^.))
import qualified Data.HashMap.Strict as HashMap
import Data.Traversable (for)
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Core.Syntax as Core
import Malgo.Infer.TcEnv (TcEnv (TcEnv))
import qualified Malgo.Infer.TcEnv as TcEnv
import Malgo.Prelude
import Malgo.Syntax as Syn
import Malgo.Syntax.Extension
import Malgo.TypeRep (kindOf, typeConstructor)
import qualified Malgo.TypeRep as R

-- | variable and type environment
data Env = Env
  { -- | Syn variable to Core variable
    _varNameEnv :: HashMap (Id ()) Name,
    -- | Syn.Type name to Core.Type name
    _typeNameEnv :: HashMap (Id ()) Name,
    -- | [Static type rep] name to Core.Type
    _typeVarEnv :: HashMap (Id R.Type) Core.Type,
    _typeDefEnv :: HashMap Name Core.TypeDef
  }

varNameEnv :: Lens' Env (HashMap (Id ()) Name)
varNameEnv = lens _varNameEnv \e x -> e {_varNameEnv = x}

typeNameEnv :: Lens' Env (HashMap (Id ()) Name)
typeNameEnv = lens _typeNameEnv \e x -> e {_typeNameEnv = x}

typeDefEnv :: Lens' Env (HashMap Name Core.TypeDef)
typeDefEnv = lens _typeDefEnv \e x -> e {_typeDefEnv = x}

lookupTypeName = undefined

initEnv :: (MonadState Env m, MonadIO m, HasUniqSupply env, MonadReader env m) => TcEnv -> m ()
initEnv TcEnv {_varEnv, _typeEnv} = do
  ifor_ _typeEnv \name typeDef -> do
    name' <- newIdOnName (dsKind (kindOf typeDef)) name
    typeNameEnv . at name ?= name'
    registerTypeConstructor (typeDef ^. typeConstructor)
  ifor_ _typeEnv \name typeDef -> do
    name' <- lookupTypeName name
    typeDef' <- dsTypeDef typeDef
    typeDefEnv . at name' ?= typeDef'
  ifor_ _varEnv \name scheme -> do
    scheme' <- dsScheme scheme
    name' <- newIdOnName scheme' name
    varNameEnv . at name ?= name'

mlgToCore :: (MonadReader env m, HasUniqSupply env, MonadIO m) => TcEnv -> Syn.Module (Malgo 'Refine) -> m Core.Module
mlgToCore tcEnv Syn.Module {_moduleDefinition = BindGroup {..}} = evaluatingStateT (Env mempty mempty mempty mempty) do
  initEnv tcEnv
  _externalDefinitions <- pure []
  _variableDefinitions <- pure []
  _typeDefinitions <- use (typeDefEnv . to HashMap.toList)
  pure $ Core.Module {..}

dsTypeDef :: Monad m => R.TypeDef R.Type -> m TypeDef
dsTypeDef R.TypeDef {..} = do
  _parameters <- traverse dsTypeParameter _typeParameters
  _constructors <- traverse dsValueConstructor _valueConstructors
  pure $ TypeDef {..}
  where
    dsValueConstructor :: (Id (), R.Scheme R.Type) -> m Name
    dsValueConstructor (constr, sc) = undefined
    dsTypeParameter :: Id R.Type -> m Name
    dsTypeParameter id = undefined

dsScheme :: Monad m => R.Scheme R.Type -> m Core.Type
dsScheme = undefined

dsKind :: R.Kind -> Core.Type
dsKind (R.TYPE (R.Rep rep)) = Core.TYPE rep
dsKind (R.TyArr k1 k2) = Core.TyFun (dsKind k1) (dsKind k2)
dsKind k = errorDoc $ "invalid kind:" <+> pPrint k

registerTypeConstructor :: Monad m => R.Type -> m ()
registerTypeConstructor (R.TyVar x) = undefined

dsExp :: Syn.Exp (Malgo 'Refine) -> Core.Type -> Core.Exp
dsExp e expected = undefined

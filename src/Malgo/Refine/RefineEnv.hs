module Malgo.Refine.RefineEnv (RefineEnv (..), buildRefineEnv) where

import Control.Lens ((^.))
import Data.Map.Strict qualified as Map
import Malgo.Id
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Lens (signatureMap, typeDefMap, typeSynonymMap)
import Malgo.Prelude

data RefineEnv = RefineEnv
  { signatureMap :: Map Id (Scheme Type),
    typeDefEnv :: Map TypeVar (TypeDef Type),
    typeSynonymMap :: Map TypeVar ([TypeVar], Type)
  }

buildRefineEnv :: TcEnv -> RefineEnv
buildRefineEnv tcEnv =
  RefineEnv
    { signatureMap = tcEnv ^. signatureMap,
      typeDefEnv = Map.fromList $ mapMaybe f $ Map.elems (tcEnv ^. typeDefMap),
      typeSynonymMap = tcEnv ^. typeSynonymMap
    }
  where
    f :: TypeDef Type -> Maybe (TypeVar, TypeDef Type)
    f t@TypeDef {_typeConstructor = TyCon con} = Just (con, t)
    f _ = Nothing

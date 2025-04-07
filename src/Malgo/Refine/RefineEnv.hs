module Malgo.Refine.RefineEnv (RefineEnv (..), buildRefineEnv) where

import Data.Map.Strict qualified as Map
import Malgo.Id
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Prelude

data RefineEnv = RefineEnv
  { signatureMap :: Map Id (Scheme Type),
    typeDefEnv :: Map TypeVar (TypeDef Type)
  }

buildRefineEnv :: TcEnv -> RefineEnv
buildRefineEnv tcEnv =
  RefineEnv
    { signatureMap = tcEnv.signatureMap,
      typeDefEnv = Map.fromList $ mapMaybe f $ Map.elems tcEnv.typeDefMap
    }
  where
    f :: TypeDef Type -> Maybe (TypeVar, TypeDef Type)
    f t@TypeDef {typeConstructor = TyCon con} = Just (con, t)
    f _ = Nothing

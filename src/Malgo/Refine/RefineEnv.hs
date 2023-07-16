module Malgo.Refine.RefineEnv (RefineEnv (..), buildRefineEnv) where

import Data.HashMap.Strict qualified as HashMap
import Koriel.Id
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Prelude

data RefineEnv = RefineEnv
  { signatureMap :: HashMap (Id ()) (Scheme Type),
    typeDefEnv :: HashMap TypeVar (TypeDef Type)
  }

buildRefineEnv :: TcEnv -> RefineEnv
buildRefineEnv TcEnv {_signatureMap, _typeDefMap} =
  RefineEnv
    { signatureMap = _signatureMap,
      typeDefEnv = HashMap.fromList $ mapMaybe f $ HashMap.elems _typeDefMap
    }
  where
    f :: TypeDef Type -> Maybe (TypeVar, TypeDef Type)
    f t@TypeDef {_typeConstructor = TyCon con} = Just (con, t)
    f _ = Nothing

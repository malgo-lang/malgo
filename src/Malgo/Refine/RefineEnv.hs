module Malgo.Refine.RefineEnv (RefineEnv (..), buildRefineEnv) where

import Data.HashMap.Strict qualified as HashMap
import Koriel.Id
import Koriel.MonadUniq
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Monad
import Malgo.Prelude

data RefineEnv = RefineEnv
  { signatureMap :: HashMap (Id ()) (Scheme Type),
    typeDefEnv :: HashMap TypeVar (TypeDef Type),
    uniqSupply :: UniqSupply
  }

buildRefineEnv :: MalgoEnv -> TcEnv -> RefineEnv
buildRefineEnv malgoEnv TcEnv {_signatureMap, _typeDefMap} =
  RefineEnv
    { signatureMap = _signatureMap,
      typeDefEnv = HashMap.fromList $ mapMaybe f $ HashMap.elems _typeDefMap,
      uniqSupply = malgoEnv.uniqSupply
    }
  where
    f :: TypeDef Type -> Maybe (TypeVar, TypeDef Type)
    f t@TypeDef {_typeConstructor = TyCon con} = Just (con, t)
    f _ = Nothing

module Malgo.Refine.RefineEnv (RefineEnv (..), buildRefineEnv) where

import Control.Lens ((^.))
import Data.HashMap.Strict qualified as HashMap
import Malgo.Id
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Lens (signatureMap, typeDefMap)
import Malgo.Prelude

data RefineEnv = RefineEnv
  { signatureMap :: HashMap Id (Scheme Type),
    typeDefEnv :: HashMap TypeVar (TypeDef Type)
  }

buildRefineEnv :: TcEnv -> RefineEnv
buildRefineEnv tcEnv =
  RefineEnv
    { signatureMap = tcEnv ^. signatureMap,
      typeDefEnv = HashMap.fromList $ mapMaybe f $ HashMap.elems (tcEnv ^. typeDefMap)
    }
  where
    f :: TypeDef Type -> Maybe (TypeVar, TypeDef Type)
    f t@TypeDef {_typeConstructor = TyCon con} = Just (con, t)
    f _ = Nothing

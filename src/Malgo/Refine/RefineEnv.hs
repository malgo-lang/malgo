module Malgo.Refine.RefineEnv (RefineEnv (..), buildRefineEnv) where

import Data.HashMap.Strict qualified as HashMap
-- import Malgo.Infer.TcEnv

import GHC.Records (HasField)
import Koriel.Id
import Koriel.MonadUniq
import Malgo.Infer.TypeRep
import Malgo.Prelude

data RefineEnv = RefineEnv
  { signatureMap :: HashMap (Id ()) (Scheme Type),
    typeDefEnv :: HashMap TypeVar (TypeDef Type),
    uniqSupply :: UniqSupply
  }

buildRefineEnv ::
  ( HasUniqSupply m,
    HasField "_signatureMap" t (HashMap (Id ()) (Scheme Type)),
    HasField "_typeDefMap" t (HashMap (Id ()) (TypeDef Type))
  ) =>
  m ->
  t ->
  RefineEnv
buildRefineEnv malgoEnv tcEnv =
  RefineEnv
    { signatureMap = tcEnv._signatureMap,
      typeDefEnv = HashMap.fromList $ mapMaybe f $ HashMap.elems tcEnv._typeDefMap,
      uniqSupply = malgoEnv.uniqSupply
    }
  where
    f :: TypeDef Type -> Maybe (TypeVar, TypeDef Type)
    f t@TypeDef {_typeConstructor = TyCon con} = Just (con, t)
    f _ = Nothing

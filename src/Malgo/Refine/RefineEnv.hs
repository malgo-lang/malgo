module Malgo.Refine.RefineEnv where

import Control.Lens (Lens', lens)
import qualified Data.HashMap.Strict as HashMap
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Malgo.Prelude
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep

data RefineEnv = RefineEnv
  { _typeDefEnv :: HashMap (Id Kind) (TypeDef Type),
    _refineMalgoEnv :: MalgoEnv
  }

typeDefEnv :: Lens' RefineEnv (HashMap (Id Kind) (TypeDef Type))
typeDefEnv = lens _typeDefEnv (\r x -> r {_typeDefEnv = x})

refineMalgoEnv :: Lens' RefineEnv MalgoEnv
refineMalgoEnv = lens _refineMalgoEnv (\r x -> r {_refineMalgoEnv = x})

instance HasMalgoEnv RefineEnv where
  malgoEnv = refineMalgoEnv

instance HasSrcName RefineEnv FilePath where
  srcName = refineMalgoEnv . srcName

instance HasUniqSupply RefineEnv UniqSupply where
  uniqSupply = refineMalgoEnv . uniqSupply

buildRefineEnv :: MalgoEnv -> TcEnv -> RefineEnv
buildRefineEnv malgoEnv TcEnv {_typeDefMap} =
  RefineEnv
    { _typeDefEnv = HashMap.fromList $ mapMaybe f $ HashMap.elems _typeDefMap,
      _refineMalgoEnv = malgoEnv
    }
  where
    f :: TypeDef Type -> Maybe (Id Kind, TypeDef Type)
    f t@TypeDef {_typeConstructor = TyCon con} = Just (con, t)
    f _ = Nothing

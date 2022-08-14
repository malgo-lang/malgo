{-# LANGUAGE TemplateHaskell #-}

module Malgo.Refine.RefineEnv where

import Control.Lens (makeFieldsNoPrefix, (^.))
import Data.HashMap.Strict qualified as HashMap
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Prelude

data RefineEnv = RefineEnv
  { _signatureMap :: HashMap (Id ()) (Scheme Type),
    _typeDefEnv :: HashMap (Id Kind) (TypeDef Type),
    _srcName :: FilePath,
    _uniqSupply :: UniqSupply
  }

makeFieldsNoPrefix ''RefineEnv

buildRefineEnv :: MalgoEnv -> TcEnv -> RefineEnv
buildRefineEnv malgoEnv TcEnv {_signatureMap, _typeDefMap} =
  RefineEnv
    { _signatureMap = _signatureMap,
      _typeDefEnv = HashMap.fromList $ mapMaybe f $ HashMap.elems _typeDefMap,
      _srcName = malgoEnv ^. srcName,
      _uniqSupply = malgoEnv ^. uniqSupply
    }
  where
    f :: TypeDef Type -> Maybe (Id Kind, TypeDef Type)
    f t@TypeDef {_typeConstructor = TyCon con} = Just (con, t)
    f _ = Nothing

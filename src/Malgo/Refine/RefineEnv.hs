{-# LANGUAGE TemplateHaskell #-}

module Malgo.Refine.RefineEnv where

import Control.Lens (makeFieldsNoPrefix, (^.))
import qualified Data.HashMap.Strict as HashMap
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Malgo.Prelude
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep

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

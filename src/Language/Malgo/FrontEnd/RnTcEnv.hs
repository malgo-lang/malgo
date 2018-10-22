{-# LANGUAGE TemplateHaskell #-}
module Language.Malgo.FrontEnd.RnTcEnv where

import           Control.Lens.TH
import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Type
import           Universum            hiding (Type)

data RnTcEnv = RnTcEnv
  { _toplevelMap  :: Map Id (TypeScheme Id)
  , _builtInMap   :: Map Text Id
  , _typeAliasMap :: Map Text ([Id], Type Id)
  }

makeLenses ''RnTcEnv

makeRnTcEnv :: MalgoM RnTcEnv
makeRnTcEnv = undefined

{-# LANGUAGE TemplateHaskell #-}

module Malgo.Module (ModuleName (..), HasModuleName) where

import Data.Aeson
import Data.Data
import Data.Store ()
import Data.Store.TH
import GHC.Records
import Malgo.Prelude

newtype ModuleName = ModuleName {raw :: Text}
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving newtype (Hashable, Pretty, ToJSON, FromJSON)

makeStore ''ModuleName

type HasModuleName r = HasField "moduleName" r ModuleName

instance HasField "moduleName" ModuleName ModuleName where
  getField = identity

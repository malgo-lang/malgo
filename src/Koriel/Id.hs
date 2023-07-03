{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Koriel.Id
  ( IdSort (..),
    ModuleName (..),
    Id (..),
    idToText,
    newInternalId,
    newExternalId,
    idIsExternal,
    newTemporalId,
    newNativeId,
    idIsNative,
    HasModuleName,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
import GHC.Records
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList)
import Koriel.Pretty as P

newtype ModuleName = ModuleName {raw :: Text}
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving newtype (Hashable, Binary, Pretty, ToJSON, FromJSON)

type HasModuleName r = HasField "moduleName" r ModuleName

instance HasField "moduleName" ModuleName ModuleName where
  getField = identity

-- | Identifier sort.
data IdSort
  = -- | Identifiers that can be referenced by other `External` modules.
    -- They are printed as `ModuleName.idName`.
    External
  | -- | Identifiers closed in the module. They are not visible outside (so `Internal`) the module.
    -- They are printed as `idName_deadbeaf`.
    Internal
  | -- | Temporary variable identifiers generated by the compiler.
    -- For dev experience, these are printed with a prefix `$`.
    Temporal
  | -- | Native identifiers, e.g. `main` that is the entry point function name.
    --   These are only generated by the compiler.
    --   These are `Native`, so they are printed raw. No prefix and no postfix.
    Native
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)
  deriving anyclass (Hashable, Binary, ToJSON, FromJSON)
  deriving (Pretty) via PrettyShow IdSort

-- TODO: Add uniq :: Int field
data Id a = Id
  { name :: Text,
    meta :: a,
    moduleName :: ModuleName,
    uniq :: Int, -- Unique number for each Id. If sort == Native or External, uniq is always -1.
    sort :: IdSort
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Data, Typeable)
  deriving anyclass (Hashable, Binary, ToJSON, FromJSON)

instance Pretty (Id a) where
  pPrint Id {name, moduleName, sort = External} = "@" <> brackets (pPrint moduleName <+> pPrint name)
  pPrint Id {name, moduleName, uniq, sort = Internal} = "#" <> brackets (pPrint moduleName <+> pPrint name <+> pPrint uniq)
  pPrint Id {name, moduleName, uniq, sort = Temporal} = "$" <> brackets (pPrint moduleName <+> pPrint name <+> pPrint uniq)
  pPrint Id {name, sort = Native} = "%" <> pPrint name

idToText :: Id a -> Text
idToText Id {name, moduleName, sort = External} = moduleName.raw <> "." <> name
idToText Id {name, moduleName, uniq, sort = Internal} = moduleName.raw <> ".#" <> name <> "_" <> convertString (show uniq)
idToText Id {name, moduleName, uniq, sort = Temporal} = moduleName.raw <> ".$" <> name <> "_" <> convertString (show uniq)
idToText Id {name, sort = Native} = name

newTemporalId :: (MonadReader env m, MonadIO m, HasUniqSupply env, HasModuleName env) => Text -> a -> m (Id a)
newTemporalId name meta = do
  uniq <- getUniq
  moduleName <- asks (.moduleName)
  let sort = Temporal
  pure Id {..}

newInternalId :: (MonadIO f, MonadReader env f, HasUniqSupply env, HasModuleName env) => Text -> a -> f (Id a)
newInternalId name meta = do
  uniq <- getUniq
  moduleName <- asks (.moduleName)
  let sort = Internal
  pure Id {..}

newExternalId :: (MonadReader env f, HasModuleName env) => Text -> a -> f (Id a)
newExternalId name meta = do
  moduleName <- asks (.moduleName)
  let uniq = -1
  let sort = External
  pure Id {..}

newNativeId :: (MonadReader env f, HasModuleName env) => Text -> a -> f (Id a)
newNativeId name meta = do
  moduleName <- asks (.moduleName)
  let uniq = -1
  let sort = Native
  pure Id {..}

idIsExternal :: Id a -> Bool
idIsExternal Id {sort = External} = True
idIsExternal _ = False

idIsNative :: Id a -> Bool
idIsNative Id {sort = Native} = True
idIsNative _ = False

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Koriel.Id
  ( IdSort (..),
    ModuleName (..),
    HasModuleName (..),
    Id (..),
    idToText,
    newInternalId,
    newExternalId,
    noName,
    idIsExternal,
    newIdOnName,
    cloneId,
    newNoNameId,
    newTemporalId,
    newNativeId,
    idIsNative,
  )
where

import Codec.Serialise
import Control.Lens (Lens', view)
import Data.Aeson
import Data.Binary (Binary)
import Data.Data (Data)
import Data.Store (Store)
import Data.String.Conversions (convertString)
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList)
import Koriel.Pretty as P
import Numeric (showHex)

newtype ModuleName = ModuleName {raw :: Text}
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary ModuleName

instance ToJSON ModuleName

instance FromJSON ModuleName

instance Hashable ModuleName

instance Serialise ModuleName

instance Store ModuleName

instance Pretty ModuleName where
  pPrint (ModuleName modName) = pPrint modName

class HasModuleName s a | s -> a where
  moduleName :: Lens' s a

instance HasModuleName ModuleName ModuleName where
  moduleName = identity

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

instance Binary IdSort

instance ToJSON IdSort

instance FromJSON IdSort

instance Hashable IdSort

instance Serialise IdSort

instance Store IdSort

instance Pretty IdSort where
  pPrint = P.text . show

data Id a = Id
  { name :: Text,
    uniq :: Int,
    meta :: a,
    moduleName :: ModuleName,
    sort :: IdSort
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance Hashable a => Hashable (Id a)

instance Binary a => Binary (Id a)

instance ToJSON a => ToJSON (Id a)

instance ToJSON a => ToJSONKey (Id a)

instance FromJSON a => FromJSON (Id a)

instance FromJSON a => FromJSONKey (Id a)

instance Serialise a => Serialise (Id a)

instance Store a => Store (Id a)

noName :: Text
noName = "noName"

idToText :: Id a -> Text
idToText id@Id {moduleName, sort = Internal} = moduleName.raw <> "." <> convertString (render $ pPrint id)
idToText id@Id {moduleName, sort = Temporal} = moduleName.raw <> "." <> convertString (render $ pPrint id)
idToText id = convertString $ render $ pPrint id

instance Pretty (Id a) where
  pPrint Id {name, moduleName, sort = External} = pPrint moduleName <> "." <> pPrint name
  pPrint Id {name, uniq, sort = Internal} = pPrint name <> "_" <> pPrint uniq
  pPrint Id {name, uniq, sort = Temporal} = pPrint $ "$" <> name <> "_" <> toText (showHex uniq "")
  pPrint Id {name, sort = Native} = pPrint name

newNoNameId :: (MonadIO f, HasUniqSupply env UniqSupply, HasModuleName env ModuleName, MonadReader env f) => a -> IdSort -> f (Id a)
newNoNameId meta sort = do
  let name = noName
  uniq <- getUniq
  moduleName <- view moduleName
  pure Id {..}

newTemporalId :: (MonadReader s m, MonadIO m, HasUniqSupply s UniqSupply, HasModuleName s ModuleName) => Text -> a -> m (Id a)
newTemporalId name meta = do
  uniq <- getUniq
  moduleName <- view moduleName
  let sort = Temporal
  pure Id {..}

newInternalId :: (MonadIO f, HasUniqSupply env UniqSupply, HasModuleName env ModuleName, MonadReader env f) => Text -> a -> f (Id a)
newInternalId name meta = do
  uniq <- getUniq
  moduleName <- view moduleName
  let sort = Internal
  pure Id {..}

newExternalId :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => Text -> a -> ModuleName -> f (Id a)
newExternalId name meta moduleName = do
  uniq <- getUniq
  let sort = External
  pure Id {..}

newNativeId :: (MonadIO f, HasUniqSupply env UniqSupply, HasModuleName env ModuleName, MonadReader env f) => Text -> a -> f (Id a)
newNativeId name meta = do
  uniq <- getUniq
  moduleName <- view moduleName
  let sort = Native
  pure Id {..}

newIdOnName :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => a -> Id b -> f (Id a)
newIdOnName meta Id {name, moduleName, sort} = do
  uniq <- getUniq
  pure Id {..}

cloneId :: (MonadIO m, HasUniqSupply env UniqSupply, MonadReader env m) => Id a -> m (Id a)
cloneId Id {..} = do
  uniq <- getUniq
  pure Id {..}

idIsExternal :: Id a -> Bool
idIsExternal Id {sort = External} = True
idIsExternal _ = False

idIsNative :: Id a -> Bool
idIsNative Id {sort = Native} = True
idIsNative _ = False

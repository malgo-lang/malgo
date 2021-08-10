{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Koriel.Id
  ( IdSort (..),
    ModuleName (..),
    _ModuleName,
    Id (..),
    idName,
    idUniq,
    idMeta,
    idSort,
    newId,
    newLocalId,
    newGlobalId,
    nameToString,
    pprIdName,
    idIsExternal,
    newIdOnSort,
    newIdOnName,
    cloneId,
    newNoNameId,
  )
where

import Data.Binary (Binary)
import Data.Deriving
import Data.Hashable (Hashable (hashWithSalt))
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList, (.=))
import Koriel.Pretty

newtype ModuleName = ModuleName String
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary ModuleName

instance Pretty ModuleName where
  pretty (ModuleName modName) = pretty modName

makePrisms ''ModuleName

data IdSort
  = -- | 他のモジュールから参照可能な識別子
    External ModuleName
  | -- | モジュール内に閉じた識別子
    Internal
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary IdSort

instance Pretty IdSort where
  pretty (External modName) = "External" <+> pretty modName
  pretty Internal = "Internal"

data Id a = Id
  { _idName :: Maybe String,
    _idUniq :: Int,
    _idMeta :: a,
    _idSort :: IdSort
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Data, Typeable)

deriveEq1 ''Id
deriveOrd1 ''Id
deriveShow1 ''Id

-- TODO: calculate hash from idUniq
instance Hashable (Id a) where
  hashWithSalt salt Id {_idUniq} = hashWithSalt salt _idUniq

instance Binary a => Binary (Id a)

nameToString :: Maybe String -> String
nameToString = fromMaybe "$NoName"

pprIdName :: Id a -> Doc ann
pprIdName Id {_idName} = pretty $ nameToString _idName

prettyMeta :: (t -> Doc ann) -> t -> Doc ann

#ifdef DEBUG
prettyMeta ppr x = braces (ppr x)
#else
prettyMeta _ _ = mempty
#endif

instance Pretty a => Pretty (Id a) where
  pretty id@(Id _ _ m (External modName)) = pretty modName <> "." <> pprIdName id <> prettyMeta pretty m
  pretty id@(Id _ u m Internal) = pprIdName id <> "_" <> pretty u <> prettyMeta pretty m

makeLenses ''Id

newId :: (MonadIO f, HasUniqSupply env, MonadReader env f) => String -> a -> IdSort -> f (Id a)
newId n m s = Id (Just n) <$> getUniq <*> pure m <*> pure s

newNoNameId :: (MonadIO f, HasUniqSupply env, MonadReader env f) => a -> IdSort -> f (Id a)
newNoNameId m s = Id Nothing <$> getUniq <*> pure m <*> pure s

newLocalId :: (MonadIO f, HasUniqSupply env, MonadReader env f) => String -> a -> f (Id a)
newLocalId n m = Id (Just n) <$> getUniq <*> pure m <*> pure Internal

newGlobalId :: (MonadIO f, HasUniqSupply env, MonadReader env f) => String -> a -> ModuleName -> f (Id a)
newGlobalId n m modName = Id (Just n) <$> getUniq <*> pure m <*> pure (External modName)

newIdOnSort :: (MonadIO f, HasUniqSupply env, MonadReader env f) => String -> a -> Id b -> f (Id a)
newIdOnSort name meta Id {_idSort} = newId name meta _idSort

newIdOnName :: (MonadIO f, HasUniqSupply env, MonadReader env f) => a -> Id b -> f (Id a)
newIdOnName meta Id {_idName, _idSort} = Id _idName <$> getUniq <*> pure meta <*> pure _idSort

cloneId :: (MonadIO m, HasUniqSupply env, MonadReader env m) => Id a -> m (Id a)
cloneId Id {..} = do
  _idUniq <- getUniq
  pure Id {_idName, _idUniq, _idMeta, _idSort}

idIsExternal :: Id a -> Bool
idIsExternal Id {_idSort = External _} = True
idIsExternal _ = False

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
    idToString,
    newInternalId,
    newExternalId,
    noName,
    pprIdName,
    idIsExternal,
    newIdOnName,
    cloneId,
    newNoNameId,
  )
where

import Data.Binary (Binary)
import Data.Deriving
import Data.Hashable (Hashable (hashWithSalt))
import GHC.Exts
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList, (.=))
import Koriel.Pretty

newtype ModuleName = ModuleName String
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary ModuleName

instance Pretty ModuleName where
  pPrint (ModuleName modName) = text modName

makePrisms ''ModuleName

data IdSort
  = -- | 他のモジュールから参照可能な識別子
    External ModuleName
  | -- | モジュール内に閉じた識別子
    Internal
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary IdSort

instance Pretty IdSort where
  pPrint (External modName) = "External" <+> pPrint modName
  pPrint Internal = "Internal"

data Id a = Id
  { _idName :: String,
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

noName :: String
noName = "$noName"

pprIdName :: Id a -> Doc
pprIdName Id {_idName} = text _idName

idToString :: Id a -> String
idToString Id {_idName, _idSort = External modName} = coerce modName <> "." <> _idName
idToString Id {_idName, _idUniq, _idSort = Internal} = _idName <> "_" <> show _idUniq

pPrintMeta :: (t -> Doc) -> t -> Doc

#ifdef DEBUG
pPrintMeta ppr x = braces (ppr x)
#else
pPrintMeta _ _ = mempty
#endif

instance Pretty a => Pretty (Id a) where
  pPrint id@(Id _ _ m (External modName)) = pPrint modName <> "." <> pprIdName id <> pPrintMeta pPrint m
  pPrint id@(Id _ u m Internal) = pprIdName id <> "_" <> text (show u) <> pPrintMeta pPrint m

makeLenses ''Id

newNoNameId :: (MonadIO f, HasUniqSupply env, MonadReader env f) => a -> IdSort -> f (Id a)
newNoNameId m s = Id noName <$> getUniq <*> pure m <*> pure s

newInternalId :: (MonadIO f, HasUniqSupply env, MonadReader env f) => String -> a -> f (Id a)
newInternalId n m = Id n <$> getUniq <*> pure m <*> pure Internal

newExternalId :: (MonadIO f, HasUniqSupply env, MonadReader env f) => String -> a -> ModuleName -> f (Id a)
newExternalId n m modName = Id n <$> getUniq <*> pure m <*> pure (External modName)

newIdOnName :: (MonadIO f, HasUniqSupply env, MonadReader env f) => a -> Id b -> f (Id a)
newIdOnName meta Id {_idName, _idSort} = Id _idName <$> getUniq <*> pure meta <*> pure _idSort

cloneId :: (MonadIO m, HasUniqSupply env, MonadReader env m) => Id a -> m (Id a)
cloneId Id {..} = do
  _idUniq <- getUniq
  pure Id {_idName, _idUniq, _idMeta, _idSort}

idIsExternal :: Id a -> Bool
idIsExternal Id {_idSort = External _} = True
idIsExternal _ = False

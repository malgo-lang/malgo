{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Koriel.Id
  ( IdSort (..),
    ModuleName (..),
    Id (..),
    idToText,
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

import Control.Lens.TH
import Data.Binary (Binary)
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Exts
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList)
import Koriel.Pretty
import Text.Show (showString, showsPrec)

newtype ModuleName = ModuleName Text
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary ModuleName

instance Hashable ModuleName

instance Pretty ModuleName where
  pPrint (ModuleName modName) = pPrint modName

data IdSort
  = -- | 他のモジュールから参照可能な識別子
    External ModuleName
  | -- | モジュール内に閉じた識別子
    Internal
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary IdSort

instance Hashable IdSort

instance Pretty IdSort where
  pPrint (External modName) = "External" <+> pPrint modName
  pPrint Internal = "Internal"

data Id a = Id
  { _name :: Text,
    _uniq :: Int,
    _meta :: a,
    _sort :: IdSort
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance Eq1 Id where
  liftEq eq id1 id2 = _name id1 == _name id2 && _uniq id1 == _uniq id2 && eq (_meta id1) (_meta id2) && _sort id1 == _sort id2

instance Ord1 Id where
  liftCompare cmp id1 id2 = compare (_name id1) (_name id2) <> compare (_uniq id1) (_uniq id2) <> cmp (_meta id1) (_meta id2) <> compare (_sort id1) (_sort id2)

instance Show1 Id where
  liftShowsPrec showPrec _ d Id {..} = showString "Id " . showsPrec d _name . showString " " . showsPrec d _uniq . showString " " . showPrec d _meta . showString " " . showsPrec d _sort

-- TODO: calculate hash from idUniq
instance Eq a => Hashable (Id a) where
  hashWithSalt salt Id {_uniq} = hashWithSalt salt _uniq

instance Binary a => Binary (Id a)

noName :: Text
noName = "$noName"

pprIdName :: Id a -> Doc
pprIdName Id {_name} = pPrint _name

idToText :: Id a -> Text
idToText Id {_name, _sort = External (ModuleName modName)} = modName <> "." <> _name
idToText Id {_name, _uniq, _sort = Internal} = _name <> "_" <> show _uniq

pPrintMeta :: (t -> Doc) -> t -> Doc

#ifdef DEBUG
pPrintMeta ppr x = braces (ppr x)
#else
pPrintMeta _ _ = mempty
#endif

instance Pretty a => Pretty (Id a) where
  pPrint id@(Id _ _ m (External _)) = pprIdName id <> pPrintMeta pPrint m
  pPrint id@(Id _ u m _) = pprIdName id <> "_" <> text (show u) <> pPrintMeta pPrint m

makeFieldsNoPrefix ''Id

newNoNameId :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => a -> IdSort -> f (Id a)
newNoNameId m s = Id noName <$> getUniq <*> pure m <*> pure s

newInternalId :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => Text -> a -> f (Id a)
newInternalId n m = Id n <$> getUniq <*> pure m <*> pure Internal

newExternalId :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => Text -> a -> ModuleName -> f (Id a)
newExternalId n m modName = Id n <$> getUniq <*> pure m <*> pure (External modName)

newIdOnName :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => a -> Id b -> f (Id a)
newIdOnName meta Id {_name, _sort} = Id _name <$> getUniq <*> pure meta <*> pure _sort

cloneId :: (MonadIO m, HasUniqSupply env UniqSupply, MonadReader env m) => Id a -> m (Id a)
cloneId Id {..} = do
  _idUniq <- getUniq
  pure Id {_name, _uniq, _meta, _sort}

idIsExternal :: Id a -> Bool
idIsExternal Id {_sort = External _} = True
idIsExternal _ = False

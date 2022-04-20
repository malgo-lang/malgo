{-# LANGUAGE CPP #-}

module Koriel.Id
  ( IdSort (..),
    ModuleName (..),
    Id (..),
    idName,
    idUniq,
    idMeta,
    idSort,
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

import Control.Lens (Lens, Lens', lens)
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
  { _idName :: Text,
    _idUniq :: Int,
    _idMeta :: a,
    _idSort :: IdSort
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance Eq1 Id where
  liftEq eq id1 id2 = _idName id1 == _idName id2 && _idUniq id1 == _idUniq id2 && eq (_idMeta id1) (_idMeta id2) && _idSort id1 == _idSort id2

instance Ord1 Id where
  liftCompare cmp id1 id2 = compare (_idName id1) (_idName id2) <> compare (_idUniq id1) (_idUniq id2) <> cmp (_idMeta id1) (_idMeta id2) <> compare (_idSort id1) (_idSort id2)

instance Show1 Id where
  liftShowsPrec showPrec _ d Id {..} = showString "Id " . showsPrec d _idName . showString " " . showsPrec d _idUniq . showString " " . showPrec d _idMeta . showString " " . showsPrec d _idSort

-- TODO: calculate hash from idUniq
instance Eq a => Hashable (Id a) where
  hashWithSalt salt Id {_idUniq} = hashWithSalt salt _idUniq

instance Binary a => Binary (Id a)

noName :: Text
noName = "$noName"

pprIdName :: Id a -> Doc
pprIdName Id {_idName} = pPrint _idName

idToText :: Id a -> Text
idToText Id {_idName, _idSort = External (ModuleName modName)} = modName <> "." <> _idName
idToText Id {_idName, _idUniq, _idSort = Internal} = _idName <> "_" <> show _idUniq

pPrintMeta :: (t -> Doc) -> t -> Doc

#ifdef DEBUG
pPrintMeta ppr x = braces (ppr x)
#else
pPrintMeta _ _ = mempty
#endif

instance Pretty a => Pretty (Id a) where
  pPrint id@(Id _ _ m (External _)) = pprIdName id <> pPrintMeta pPrint m
  pPrint id@(Id _ u m _) = pprIdName id <> "_" <> text (show u) <> pPrintMeta pPrint m

idName :: Lens' (Id a) Text
idName = lens _idName (\i x -> i {_idName = x})

idUniq :: Lens' (Id a) Int
idUniq = lens _idUniq (\i x -> i {_idUniq = x})

idMeta :: Lens (Id a) (Id b) a b
idMeta = lens _idMeta (\i x -> i {_idMeta = x})

idSort :: Lens' (Id a) IdSort
idSort = lens _idSort (\i x -> i {_idSort = x})

newNoNameId :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => a -> IdSort -> f (Id a)
newNoNameId m s = Id noName <$> getUniq <*> pure m <*> pure s

newInternalId :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => Text -> a -> f (Id a)
newInternalId n m = Id n <$> getUniq <*> pure m <*> pure Internal

newExternalId :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => Text -> a -> ModuleName -> f (Id a)
newExternalId n m modName = Id n <$> getUniq <*> pure m <*> pure (External modName)

newIdOnName :: (MonadIO f, HasUniqSupply env UniqSupply, MonadReader env f) => a -> Id b -> f (Id a)
newIdOnName meta Id {_idName, _idSort} = Id _idName <$> getUniq <*> pure meta <*> pure _idSort

cloneId :: (MonadIO m, HasUniqSupply env UniqSupply, MonadReader env m) => Id a -> m (Id a)
cloneId Id {..} = do
  _idUniq <- getUniq
  pure Id {_idName, _idUniq, _idMeta, _idSort}

idIsExternal :: Id a -> Bool
idIsExternal Id {_idSort = External _} = True
idIsExternal _ = False

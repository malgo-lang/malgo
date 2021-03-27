{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
    pprIdName,
    idIsExternal,
    idIsWiredIn,
    newIdOnSort,
    newIdOnName,
    cloneId,
  )
where

import Data.Binary (Binary)
import Data.Deriving
import Data.Hashable (Hashable (hashWithSalt))
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList, (.=))
import Koriel.Pretty

-- TODO: IdSortに求められる性質
-- 1. Idが外のモジュールから参照できるか否か（Interface）
-- 1. 関数がLLVMレベルで外のファイルから見えるか（internalか否か）（CodeGen）
-- 1. Idがモジュール名で修飾されているかいなか（mkName, mainFunc）
-- 1. LLVMレベルでトップレベルで宣言されているか否か
-- これらを整理する必要がある。
data IdSort
  = External ModuleName
  | WiredIn ModuleName
  | Internal
  deriving stock (Eq, Show, Ord, Generic)

instance Binary IdSort

instance Pretty IdSort where
  pPrint (External modName) = "External" <+> pPrint modName
  pPrint (WiredIn modName) = "WiredIn" <+> pPrint modName
  pPrint Internal = "Internal"

newtype ModuleName = ModuleName String
  deriving stock (Eq, Show, Ord, Generic)

instance Pretty ModuleName where
  pPrint (ModuleName modName) = text modName

instance Binary ModuleName

makePrisms ''ModuleName

data Id a = Id
  { _idName :: String,
    _idUniq :: Int,
    _idMeta :: a,
    _idSort :: IdSort
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

deriveEq1 ''Id
deriveOrd1 ''Id
deriveShow1 ''Id

-- TODO: calculate hash from idUniq
instance Hashable (Id a) where
  hashWithSalt salt Id {_idUniq} = hashWithSalt salt _idUniq

instance Binary a => Binary (Id a)

pprIdName :: Id a -> Doc
pprIdName Id {_idName} = text _idName

pPrintMeta :: (t -> Doc) -> t -> Doc

#ifdef DEBUG
pPrintMeta ppr x = braces (ppr x)
#else
pPrintMeta _ _ = mempty
#endif

instance Pretty a => Pretty (Id a) where
  pPrint (Id n _ m (External modName)) = pPrint modName <> "." <> text n <> pPrintMeta pPrint m
  pPrint (Id n _ m (WiredIn modName)) = pPrint modName <> "." <> text n <> pPrintMeta pPrint m
  pPrint (Id n u m Internal) = text n <> "_" <> text (show u) <> pPrintMeta pPrint m

instance Pretty1 Id where
  liftPPrintPrec ppr l d (Id n _ m (External modName)) = pPrint modName <> "." <> text n <> pPrintMeta (ppr l d) m
  liftPPrintPrec ppr l d (Id n _ m (WiredIn modName)) = pPrint modName <> "." <> text n <> pPrintMeta (ppr l d) m
  liftPPrintPrec ppr l d (Id n u m Internal) = text n <> "_" <> text (show u) <> pPrintMeta (ppr l d) m

makeLenses ''Id

newId :: MonadUniq f => String -> a -> IdSort -> f (Id a)
newId n m s = Id n <$> getUniq <*> pure m <*> pure s

newLocalId :: MonadUniq f => String -> a -> f (Id a)
newLocalId n m = Id n <$> getUniq <*> pure m <*> pure Internal

newGlobalId :: MonadUniq f => String -> a -> ModuleName -> f (Id a)
newGlobalId n m modName = Id n <$> getUniq <*> pure m <*> pure (External modName)

newIdOnSort :: MonadUniq f => String -> a -> Id b -> f (Id a)
newIdOnSort name meta Id {_idSort} = newId name meta _idSort

newIdOnName :: MonadUniq f => a -> Id b -> f (Id a)
newIdOnName meta Id {_idName, _idSort} = newId _idName meta _idSort

cloneId :: MonadUniq m => Id a -> m (Id a)
cloneId Id {..} = do
  _idUniq <- getUniq
  pure Id {_idName, _idUniq, _idMeta, _idSort}

idIsExternal :: Id a -> Bool
idIsExternal Id {_idSort = External _} = True
idIsExternal _ = False

idIsWiredIn :: Id a -> Bool
idIsWiredIn Id {_idSort = WiredIn _} = True
idIsWiredIn _ = False

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Koriel.Id
  ( Id,
    idName,
    idUniq,
    idMeta,
    idIsTopLevel,
    idIsExternal,
    newId,
    newGlobalId,
    newLocalId,
    newTopLevelId,
  )
where

import Data.Aeson
import Data.Binary (Binary)
import Data.Deriving
import Data.Hashable (Hashable (hashWithSalt))
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList, (.=))
import Koriel.Pretty

data Id a = Id
  { _idName :: String,
    _idUniq :: Int,
    _idMeta :: a,
    _idIsTopLevel :: Bool,
    _idIsExternal :: Bool
  }
  deriving stock (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Generic)

deriveEq1 ''Id
deriveOrd1 ''Id
deriveShow1 ''Id

-- TODO: calculate hash from idUniq
instance Hashable (Id a) where
  hashWithSalt salt Id {_idUniq} = hashWithSalt salt _idUniq

instance Binary a => Binary (Id a)

#ifdef DEBUG
instance Pretty a => Pretty (Id a) where
  pPrint (Id n _ m _ True) = text n <> braces (pPrint m)
  pPrint (Id n u m _ False) = text n <> "." <> text (show u) <> braces (pPrint m)

instance Pretty1 Id where
  liftPPrintPrec ppr l d (Id n _ m _ True) = text n <> braces (ppr l d m)
  liftPPrintPrec ppr l d (Id n u m _ False) = text n <> "." <> text (show u) <> braces (ppr l d m)
#else
instance Pretty a => Pretty (Id a) where
  pPrint (Id n _ _ _ True) = text n
  pPrint (Id n u _ _ False) = text n <> "." <> text (show u)

instance Pretty1 Id where
  liftPPrintPrec _ _ _ (Id n _ _ _ True) = text n
  liftPPrintPrec _ _ _ (Id n u _ _ False) = text n <> "." <> text (show u)
#endif

instance ToJSON a => ToJSON (Id a) where
  toJSON Id {_idName, _idUniq, _idMeta, _idIsTopLevel, _idIsExternal} =
    object
      [ "type" .= ("Id" :: String),
        "name" .= _idName,
        "uniq" .= _idUniq,
        "meta" .= toJSON _idMeta,
        "is_toplevel" .= _idIsTopLevel,
        "is_external" .= _idIsExternal
      ]

instance FromJSON a => FromJSON (Id a) where
  parseJSON = withObject "Id" $ \v -> do
    withText "type" (guard . (== "Id")) =<< v .: "type"
    meta <- parseJSON =<< v .: "meta"
    Id <$> v .: "name" <*> v .: "uniq" <*> pure meta <*> v .: "is_toplevel" <*> v .: "is_external"

makeLenses ''Id

newId :: MonadUniq f => String -> a -> Bool -> Bool -> f (Id a)
newId n m t e = Id n <$> getUniq <*> pure m <*> pure t <*> pure e

newLocalId :: MonadUniq f => String -> a -> f (Id a)
newLocalId n m = Id n <$> getUniq <*> pure m <*> pure False <*> pure False

newTopLevelId :: MonadUniq f => String -> a -> f (Id a)
newTopLevelId n m = Id n <$> getUniq <*> pure m <*> pure True <*> pure False

newGlobalId :: MonadUniq f => String -> a -> f (Id a)
newGlobalId n m = Id n <$> getUniq <*> pure m <*> pure True <*> pure True

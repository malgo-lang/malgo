{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
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

import Data.Store (Store)
import Koriel.MonadUniq
import Koriel.Prelude hiding (toList)
import Koriel.Pretty

data Id a = Id
  { _idName :: String,
    _idUniq :: Int,
    _idMeta :: a,
    _idIsTopLevel :: Bool,
    _idIsExternal :: Bool
  }
  deriving stock (Show, Read, Eq, Ord, Functor, Foldable, Generic)

instance Store a => Store (Id a)

#ifndef DEBUG
instance Pretty a => Pretty (Id a) where
  pPrint (Id n _ _ _ True) = text n
  pPrint (Id n u _ _ False) = text n <> "." <> text (show u)
#else
instance Pretty a => Pretty (Id a) where
  pPrint (Id n _ m _ True) = text n <> braces (pPrint m)
  pPrint (Id n u m _ False) = text n <> "." <> text (show u) <> braces (pPrint m)
#endif

idName :: Getter (Id a) String
idName = to _idName

idUniq :: Getter (Id a) Int
idUniq = to _idUniq

idMeta :: Lens (Id a) (Id b) a b
idMeta = lens _idMeta (\i x -> i {_idMeta = x})

idIsTopLevel :: Lens (Id a) (Id a) Bool Bool
idIsTopLevel = lens _idIsTopLevel (\i x -> i {_idIsTopLevel = x})

idIsExternal :: Lens' (Id a) Bool
idIsExternal = lens _idIsExternal (\i x -> i {_idIsExternal = x})

newId :: MonadUniq f => String -> a -> Bool -> Bool -> f (Id a)
newId n m t e = Id n <$> getUniq <*> pure m <*> pure t <*> pure e

newLocalId :: MonadUniq f => String -> a -> f (Id a)
newLocalId n m = Id n <$> getUniq <*> pure m <*> pure False <*> pure False

newTopLevelId :: MonadUniq f => String -> a -> f (Id a)
newTopLevelId n m = Id n <$> getUniq <*> pure m <*> pure True <*> pure False

newGlobalId :: MonadUniq f => String -> a -> f (Id a)
newGlobalId n m = Id n <$> getUniq <*> pure m <*> pure True <*> pure True

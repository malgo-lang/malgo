{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE TypeFamilies               #-}
module Language.Malgo.ID
  ( ID(..)
  , newID
  , IDMap(..)
  )
where

import           Data.Data                      ( Data )
import           Data.Functor.Classes
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude  hiding ( delete )
import           Relude.Extra.Map

data ID a = ID { idName :: Text, idUniq :: Int, idMeta :: a }
  deriving (Show, Ord, Read, Functor, Foldable, Generic)

instance Eq (ID a) where
  x == y = idUniq x == idUniq y

instance Pretty a => Pretty (ID a) where
  pPrint (ID n u _) = pPrint n <> "." <> pPrint u

instance HasType a => HasType (ID a) where
  typeOf ID { idMeta } = typeOf idMeta

newID :: MonadMalgo f => a -> Text -> f (ID a)
newID m n = ID n <$> newUniq <*> pure m

newtype IDMap a v = IDMap { unwrapIDMap :: IntMap v }
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic, Data)
  deriving newtype ( Eq1, Ord1, Read1, Show1, Semigroup, Monoid, NFData)

instance One (IDMap a v) where
  type OneItem (IDMap a v) = (ID a, v)
  one (ID { idUniq }, v) = IDMap (one (idUniq, v))

instance StaticMap (IDMap a v) where
  type Key (IDMap a v) = ID a
  type Val (IDMap a v) = v
  size = size . unwrapIDMap
  lookup ID { idUniq } = lookup idUniq . unwrapIDMap
  member ID { idUniq } = member idUniq . unwrapIDMap

instance DynamicMap (IDMap a v) where
  insert ID { idUniq } v = IDMap . insert idUniq v . unwrapIDMap
  insertWith f ID { idUniq } v = IDMap . insertWith f idUniq v . unwrapIDMap
  delete ID { idUniq } = IDMap . delete idUniq . unwrapIDMap
  alter f ID { idUniq } = IDMap . alter f idUniq . unwrapIDMap

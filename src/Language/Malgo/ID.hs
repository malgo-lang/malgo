{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Malgo.ID
  ( ID(..)
  , nameL
  , uniqL
  , metaL
  , newID
--  , updateID
  , IDMap(..)
  )
where

import           Language.Malgo.Monad
import           Language.Malgo.Prelude  hiding ( toList )
import           Language.Malgo.Pretty

import           Language.Malgo.TypeRep.Type

import           Control.Lens.Combinators
import           Data.Functor.Classes
import qualified Data.IntMap                   as IntMap
import           GHC.Exts                       ( IsList(..) )
import           Text.PrettyPrint.HughesPJClass ( text )

data ID a = ID { idName :: String, idUniq :: Int, idMeta :: a }
  deriving stock (Show, Eq, Ord, Functor, Foldable)

instance Pretty a => Pretty (ID a) where
  pPrint (ID n u m) = text n <> "." <> text (show u) <> "<" <> pPrint m <> ">"

instance HasType a => HasType (ID a) where
  typeOf ID { idMeta } = typeOf idMeta

nameL :: Lens (ID a) (ID a) String String
nameL = lens idName (\i x -> i { idName = x })

uniqL :: Lens (ID a) (ID a) Int Int
uniqL = lens idUniq (\i x -> i { idUniq = x })

metaL :: Lens (ID a) (ID b) a b
metaL = lens idMeta (\i x -> i { idMeta = x })

newID :: MonadUniq f => a -> String -> f (ID a)
newID m n = ID n <$> getUniq <*> pure m

newtype IDMap a v = IDMap { unwrapIDMap :: IntMap v }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (Eq1, Ord1, Show1, Semigroup, Monoid, NFData)

instance Pretty v => Pretty (IDMap a v) where
  pPrint = pPrint . toList . unwrapIDMap

instance One (IDMap a v) where
  type OneItem (IDMap a v) = (ID a, v)
  one (ID { idUniq }, v) = IDMap (one (idUniq, v))

type instance Index (IDMap a v) = ID a
type instance IxValue (IDMap a v) = v

instance Ixed (IDMap a v)

instance At (IDMap a v) where
  at ID { idUniq } f (IDMap m) = IDMap <$> IntMap.alterF f idUniq m

instance IsList (IDMap a v) where
  type Item (IDMap a v) = (ID a, v)
  fromList = foldr (\(k, v) m -> m & set (at k) (Just v)) mempty
  toList   = error "cannot convert to list"

{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Language.Malgo.ID
  (ID(..), RawID, TypedID, idName, idUniq, idMeta, newID, IDMap(..)) where

import           Control.Lens                (makeLenses, view)
import           Data.Data                   (Data)
import           Data.Functor.Classes
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Relude                      hiding (Type)
import           Relude.Extra.Map

data ID a = ID { _idName :: Text, _idUniq :: Int, _idMeta :: a }
  deriving (Show, Ord, Read, Functor, Foldable, Generic)

instance PrettyVal a => PrettyVal (ID a)

type RawID = ID ()

type TypedID = ID Type

instance Eq (ID a) where
  x == y = _idUniq x == _idUniq y

makeLenses ''ID

ignore :: a -> b -> b
ignore = flip const

instance Pretty a => Pretty (ID a) where
  pPrint (ID n u m) =
#ifdef SHOW_META
    pPrint n <> "." <> pPrint u <> braces (pPrint m)
#else
    ignore m $ pPrint n <> "." <> pPrint u
#endif

instance HasType a => HasType (ID a) where
  typeOf i = typeOf $ view idMeta i

newID :: MonadMalgo f => a -> Text -> f (ID a)
newID m n =
  ID n <$> newUniq <*> pure m

newtype IDMap a v = IDMap { unwrapIDMap :: IntMap v }
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic, Data)
  deriving newtype ( Eq1, Ord1, Read1, Show1, Semigroup, Monoid, NFData)

instance One (IDMap a v) where
  type OneItem (IDMap a v) = (ID a, v)
  one (ID {_idUniq}, v) = IDMap (one (_idUniq, v))

instance StaticMap (IDMap a v) where
  type Key (IDMap a v) = ID a
  type Val (IDMap a v) = v
  size = size . unwrapIDMap
  lookup ID{_idUniq} = lookup _idUniq . unwrapIDMap
  member ID{_idUniq} = member _idUniq . unwrapIDMap

instance DynamicMap (IDMap a v) where
  insert ID{_idUniq} v = IDMap . insert _idUniq v . unwrapIDMap
  insertWith f ID{_idUniq} v = IDMap . insertWith f _idUniq v . unwrapIDMap
  delete ID{_idUniq} = IDMap . delete _idUniq . unwrapIDMap
  alter f ID{_idUniq} = IDMap . alter f _idUniq . unwrapIDMap

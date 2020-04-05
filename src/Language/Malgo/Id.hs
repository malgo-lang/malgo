{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE TypeFamilies               #-}
module Language.Malgo.Id
  ( Id
  , nameL
  , uniqL
  , metaL
  , newId
  , IdMap(..)
  )
where

import           Language.Malgo.Monad
import           Language.Malgo.Prelude         hiding (toList)
import           Language.Malgo.Pretty

import           Language.Malgo.TypeRep.Type

import           Data.Functor.Classes
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as IntMap
import           GHC.Exts                       (IsList (..))
import           Text.PrettyPrint.HughesPJClass (text)

data Id a = Id
    { idName :: String
    , idUniq :: Int
    , idMeta :: a
    }
    deriving stock (Show, Eq, Ord, Functor, Foldable)

instance Pretty a => Pretty (Id a) where
  pPrint (Id n u m) = text n <> "." <> text (show u) <> "<" <> pPrint m <> ">"

instance HasType a => HasType (Id a) where
  typeOf Id { idMeta } = typeOf idMeta

nameL :: Lens (Id a) (Id a) String String
nameL = lens idName (\i x -> i { idName = x })

uniqL :: Getter (Id a) Int
uniqL = lens idUniq (\i x -> i { idUniq = x })

metaL :: Lens (Id a) (Id b) a b
metaL = lens idMeta (\i x -> i { idMeta = x })

newId :: MonadUniq f => a -> String -> f (Id a)
newId m n = Id n <$> getUniq <*> pure m

newtype IdMap a v = IdMap { unwrapIdMap :: IntMap v }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (Eq1, Ord1, Show1, Semigroup, Monoid)

instance Pretty v => Pretty (IdMap a v) where
  pPrint = pPrint . toList . unwrapIdMap

type instance Index (IdMap a v) = Id a
type instance IxValue (IdMap a v) = v

instance Ixed (IdMap a v)

instance At (IdMap a v) where
  at Id { idUniq } f (IdMap m) = IdMap <$> IntMap.alterF f idUniq m

instance IsList (IdMap a v) where
  type Item (IdMap a v) = (Id a, v)
  fromList = foldr (\(k, v) m -> m & set (at k) (Just v)) mempty
  toList   = error "cannot convert to list"

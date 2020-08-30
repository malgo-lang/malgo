{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Malgo.Id
  ( Id,
    idName,
    idUniq,
    idMeta,
    newId,
    IdMap (..),
  )
where

import Data.Functor.Classes
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import GHC.Exts (IsList (..))
import Language.Malgo.Monad
import Language.Malgo.Prelude hiding (toList)
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.Type
import Text.PrettyPrint.HughesPJClass (text)
import qualified Data.Text as T

data Id a
  = Id
      { _idName :: Text,
        _idUniq :: Int,
        _idMeta :: a
      }
  deriving stock (Show, Functor, Foldable)

instance Eq (Id a) where
  Id {_idUniq = x} == Id {_idUniq = y} = x == y

instance Ord (Id a) where
  compare Id {_idUniq = x} Id {_idUniq = y} = compare x y

instance Pretty a => Pretty (Id a) where
  pPrint (Id n u _) = text (T.unpack n) <> "." <> text (show u)

instance HasType a => HasType (Id a) where
  typeOf Id {_idMeta} = typeOf _idMeta

idName :: Getter (Id a) Text
idName = lens _idName (\i x -> i {_idName = x})

idUniq :: Getter (Id a) Int
idUniq = lens _idUniq (\i x -> i {_idUniq = x})

idMeta :: Lens (Id a) (Id b) a b
idMeta = lens _idMeta (\i x -> i {_idMeta = x})

newId :: MonadUniq f => a -> Text -> f (Id a)
newId m n = Id n <$> getUniq <*> pure m

newtype IdMap a v = IdMap {unwrapIdMap :: IntMap v}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (Eq1, Ord1, Show1, Semigroup, Monoid)

instance Pretty v => Pretty (IdMap a v) where
  pPrint = pPrint . toList . unwrapIdMap

type instance Index (IdMap a v) = Id a

type instance IxValue (IdMap a v) = v

instance Ixed (IdMap a v)

instance At (IdMap a v) where
  at Id {_idUniq} f (IdMap m) = IdMap <$> IntMap.alterF f _idUniq m

instance IsList (IdMap a v) where
  type Item (IdMap a v) = (Id a, v)
  fromList = foldr (\(k, v) m -> m & set (at k) (Just v)) mempty
  toList = error "cannot convert to list"

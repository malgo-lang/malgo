{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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
  )
where

import Koriel.Prelude hiding (toList)
import Koriel.Pretty
import Language.Malgo.Monad
import Language.Malgo.TypeRep.Type

data Id a = Id
  { _idName :: String,
    _idUniq :: Int,
    _idMeta :: a
  }
  deriving stock (Show, Functor, Foldable)

instance Eq (Id a) where
  Id {_idUniq = x} == Id {_idUniq = y} = x == y

instance Ord (Id a) where
  compare Id {_idUniq = x} Id {_idUniq = y} = compare x y

instance Pretty a => Pretty (Id a) where
  pPrint (Id n u _) = text n <> "." <> text (show u)

instance HasType a => HasType (Id a) where
  typeOf Id {_idMeta} = typeOf _idMeta

idName :: Getter (Id a) String
idName = to _idName

idUniq :: Getter (Id a) Int
idUniq = to _idUniq

idMeta :: Lens (Id a) (Id b) a b
idMeta = lens _idMeta (\i x -> i {_idMeta = x})

newId :: MonadUniq f => a -> String -> f (Id a)
newId m n = Id n <$> getUniq <*> pure m

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
    newId,
    newGlobalId,
  )
where

import Koriel.MonadUniq
import Koriel.Prelude hiding (toList)
import Koriel.Pretty

data Id a = Id
  { _idName :: String,
    _idUniq :: Int,
    _idMeta :: a,
    _idIsGlobal :: Bool
  }
  deriving stock (Show, Functor, Foldable)

instance Eq (Id a) where
  Id {_idUniq = x} == Id {_idUniq = y} = x == y

instance Ord (Id a) where
  compare Id {_idUniq = x} Id {_idUniq = y} = compare x y

instance Pretty a => Pretty (Id a) where
  pPrint (Id n _ _ True) = text n
  pPrint (Id n u _ False) = text n <> "." <> text (show u)

idName :: Getter (Id a) String
idName = to _idName

idUniq :: Getter (Id a) Int
idUniq = to _idUniq

idMeta :: Lens (Id a) (Id b) a b
idMeta = lens _idMeta (\i x -> i {_idMeta = x})

newId :: MonadUniq f =>  String -> a -> f (Id a)
newId n m = Id n <$> getUniq <*> pure m <*> pure False

newGlobalId :: MonadUniq f => String -> a -> f (Id a)
newGlobalId n m = Id n <$> getUniq <*> pure m <*> pure True

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Malgo.FrontEnd.Typing.Subst
  ( Subst(..)
  , Substitutable(..)
  )
where

import           Language.Malgo.Prelude

import           Language.Malgo.TypeRep.Type

import           Data.Set                       ( (\\) )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map

newtype Subst = Subst { unwrapSubst :: Map TyVar Type }
  deriving stock (Eq, Show)
  deriving newtype (Substitutable, Monoid)

instance Ixed Subst
instance At Subst where
  at k f (Subst m) = Subst <$> Map.alterF f k m

type instance Index Subst = TyVar
type instance IxValue Subst = Type

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst $ apply (Subst s1) s2 <> s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set TyVar

instance Substitutable Scheme where
  apply s (Forall ts t) = Forall ts $ apply s' t where s' = Subst $ foldr sans (unwrapSubst s) ts
  ftv (Forall ts t) = ftv t \\ Set.fromList ts

instance Substitutable Type where
  apply s t@(TyMeta a  ) = fromMaybe t $ view (at a) s
  apply s (  TyApp c ts) = TyApp c $ apply s ts
  apply _ Kind           = Kind
  ftv (TyMeta a  ) = Set.singleton a
  ftv (TyApp _ ts) = foldMap ftv ts
  ftv Kind         = mempty

instance (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
  apply = fmap . apply
  ftv   = foldMap ftv

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

import           Language.Malgo.Prelude  hiding ( delete )

import           Language.Malgo.TypeRep.Type

import           Relude.Extra.Map

newtype Subst = Subst { unwrapSubst :: Map TyVar Type }
  deriving stock (Eq, Show)
  deriving newtype (Substitutable, Monoid)

instance StaticMap Subst where
  type Key Subst = TyVar
  type Val Subst = Type
  size (Subst s) = size s
  lookup k (Subst s) = lookup k s
  member k (Subst s) = member k s

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst $ apply (Subst s1) s2 <> s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set TyVar

instance Substitutable () where
  apply _ _ = ()
  ftv _ = mempty

instance Substitutable Scheme where
  apply s (Forall ts t) = Forall ts $ apply s' t
    where s' = Subst $ foldr delete (unwrapSubst s) ts
  ftv (Forall ts t) = ftv t \\ fromList ts

instance Substitutable Type where
  apply s t@(TyMeta a  ) = lookupDefault t a s
  apply s (  TyApp c ts) = TyApp c $ apply s ts
  apply _ Kind           = Kind
  ftv (TyMeta a  ) = one a
  ftv (TyApp _ ts) = foldMap ftv ts
  ftv Kind         = mempty

instance (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
  apply = fmap . apply
  ftv   = foldMap ftv

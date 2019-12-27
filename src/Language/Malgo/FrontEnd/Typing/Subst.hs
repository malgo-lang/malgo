{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
module Language.Malgo.FrontEnd.Typing.Subst
  ( Subst(..)
  , Substitutable(..)
  )
where

import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude
import           Relude.Extra.Map

newtype Subst = Subst (Map TyVar Type)
  deriving (Eq, Show, Substitutable, Monoid)

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

instance Substitutable Type where
  apply s t@(TyMeta a  ) = lookupDefault t a s
  apply s (  TyApp c ts) = TyApp c $ apply s ts
  apply s (TyForall ts t) = TyForall ts $ apply s t
  ftv (TyMeta a  ) = one a
  ftv (TyApp _ ts) = foldMap ftv ts
  ftv (TyForall ts t) = ftv t \\ fromList ts

instance (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
  apply = fmap . apply
  ftv   = foldMap ftv

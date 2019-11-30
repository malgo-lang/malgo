{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Malgo.FrontEnd.Typing.Subst (Subst(..), Substitutable(..)) where

import           Language.Malgo.ID
import           Language.Malgo.IR.Syntax
import           Language.Malgo.TypeRep.Type
import           Relude                      hiding (Type)
import           Relude.Extra.Map

newtype Subst = Subst (Map TyVar Type)
  deriving (Eq, Show)

instance StaticMap Subst where
  type Key Subst = TyVar
  type Val Subst = Type
  size (Subst s) = size s
  lookup k (Subst s) = lookup k s
  member k (Subst s) = member k s

compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) = Subst $ apply (Subst s1) s2 <> s1

instance Semigroup Subst where
  (<>) = compose

instance Monoid Subst where
  mempty = Subst mempty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set TyVar

instance Substitutable Type where
  apply s t@(TyMeta a) = lookupDefault t a s
  apply s (TyApp c ts) = TyApp c $ apply s ts
  ftv (TyMeta a)   = one a
  ftv (TyApp _ ts) = ftv ts

instance (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
  apply = fmap . apply
  ftv = foldr ((<>) . ftv) mempty

instance Substitutable Subst where
  apply s = apply s . coerce
  ftv = ftv @(Map TyVar Type) . coerce

instance Substitutable a => Substitutable (ID a) where
  apply s (ID name uniq meta) = ID name uniq (apply s meta)
  ftv (ID _ _ meta) = ftv meta

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Unify where

import Control.Monad.Cont (ContT)
import Control.Monad.Identity (IdentityT)
import qualified Control.Monad.State.Lazy as Lazy
import Data.Fix
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare), Show1 (liftShowsPrec))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)
import Data.Void
import GHC.Generics (Generic1)
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.TypeRep.Static (IsType (..))

-----------
-- UTerm --
-----------

data UTerm t v where
  UVar :: v -> UTerm t v
  UTerm :: t (UTerm t v) -> UTerm t v

instance (Eq v, Eq1 t) => Eq (UTerm t v) where
  (UVar v1) == (UVar v2) = v1 == v2
  (UTerm t1) == (UTerm t2) = liftEq (==) t1 t2
  _ == _ = False

instance (Ord v, Ord1 t) => Ord (UTerm t v) where
  compare (UVar v1) (UVar v2) = compare v1 v2
  compare (UTerm t1) (UTerm t2) = liftCompare compare t1 t2
  compare UVar {} UTerm {} = LT
  compare UTerm {} UVar {} = GT

instance (Show v, Show1 t) => Show (UTerm t v) where
  showsPrec d (UVar v) = showParen (d >= 11) $ showString "UVar " . showsPrec 11 v
  showsPrec d (UTerm t) = showParen (d >= 11) $ showString "UTerm " . liftShowsPrec showsPrec showList 11 t

deriving stock instance (Generic1 t, Generic v) => Generic (UTerm t v)

instance (Pretty v, Pretty1 t) => Pretty (UTerm t v) where
  pPrintPrec _ _ (UVar v) = pPrint v
  pPrintPrec l d (UTerm t) = liftPPrintPrec pPrintPrec l d t

instance IsType (t (UTerm t v)) => IsType (UTerm t v) where
  safeToType (UVar _) = Nothing
  safeToType (UTerm t) = safeToType t
  fromType t = UTerm $ fromType t

freeze :: Traversable t => UTerm t v -> Maybe (Fix t)
freeze (UVar _) = Nothing
freeze (UTerm t) = Fix <$> traverse freeze t

unfreeze :: Functor t => Fix t -> UTerm t v
unfreeze = UTerm . fmap unfreeze . unFix

freevars :: (Eq a, Foldable t, Hashable a) => UTerm t a -> HashSet a
freevars (UVar v) = HashSet.singleton v
freevars (UTerm t) = foldMap freevars t

occursCheck :: (Eq a, Foldable t, Pretty x, Pretty a, Pretty1 t, Applicative f, Hashable a) => x -> a -> UTerm t a -> f ()
occursCheck x v t =
  if HashSet.member v (freevars t)
    then errorWithMeta x $ "Occurs check:" <+> quotes (pPrint v) <+> "for" <+> pPrint t
    else pure ()

equiv :: (Eq v, Hashable v, Unifiable t v) => UTerm t v -> UTerm t v -> Maybe (HashMap v v)
equiv (UVar v1) (UVar v2)
  | v1 == v2 = Just mempty
  | otherwise = Just $ HashMap.singleton v1 v2
equiv (UTerm t1) (UTerm t2) =
  mconcat <$> traverse (\With {_value = u1 :~ u2} -> equiv u1 u2) (unify () t1 t2)
equiv _ _ = Nothing

class HasUTerm t v a where
  walkOn :: Traversal' a (UTerm t v)

instance HasUTerm t v (UTerm t v) where
  walkOn = id

instance HasUTerm t v x => HasUTerm t v (With x a) where
  walkOn f (With x a) = With <$> walkOn f x <*> pure a

instance HasUTerm t v Void where
  walkOn _ x = absurd x

----------------
-- Constraint --
----------------

infixl 5 :~

data Constraint t v = UTerm t v :~ UTerm t v
  deriving stock (Eq, Ord, Show, Generic)

instance (Pretty v, Pretty1 t) => Pretty (Constraint t v) where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

type WithMeta x t v = With x (Constraint t v)

---------------
-- Unifiable --
---------------

class Unifiable t v | t -> v where
  unify :: (Pretty x) => x -> t (UTerm t v) -> t (UTerm t v) -> [WithMeta x t v]

class Monad m => MonadBind t v m | v -> t, t -> v where
  lookupVar :: v -> m (Maybe (UTerm t v))
  default lookupVar :: (MonadTrans tr, MonadBind t v m1, m ~ tr m1) => v -> m (Maybe (UTerm t v))
  lookupVar v = lift (lookupVar v)
  freshVar :: m v
  default freshVar :: (MonadTrans tr, MonadBind t v m1, m ~ tr m1) => m v
  freshVar = lift freshVar
  newVar :: Pretty x => x -> UTerm t v -> m v
  newVar x t = do
    v <- freshVar
    bindVar x v t
    pure v
  bindVar :: Pretty x => x -> v -> UTerm t v -> m ()
  default bindVar :: (MonadTrans tr, MonadBind t v m1, m ~ tr m1, Pretty x) => x -> v -> UTerm t v -> m ()
  bindVar x v t = lift (bindVar x v t)

instance MonadBind t v m => MonadBind t v (IdentityT m)

instance MonadBind t v m => MonadBind t v (ReaderT r m)

instance MonadBind t v m => MonadBind t v (ExceptT e m)

instance MonadBind t v m => MonadBind t v (StateT s m)

instance MonadBind t v m => MonadBind t v (Lazy.StateT s m)

instance MonadBind t v m => MonadBind t v (WriterT w m)

instance MonadBind t v m => MonadBind t v (ContT r m)

------------
-- Solver --
------------

solve ::
  ( Pretty x,
    Pretty v,
    MonadBind t v m,
    Traversable t,
    Pretty1 t,
    Unifiable t v,
    Eq v
  ) =>
  [WithMeta x t v] ->
  m ()
solve cs = do
  solveLoop 5000 cs

solveLoop ::
  ( Pretty x,
    Pretty v,
    MonadBind t v m,
    Traversable t,
    Pretty1 t,
    Unifiable t v,
    Eq v
  ) =>
  Int ->
  [WithMeta x t v] ->
  m ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (With x (UVar v1 :~ UVar v2) : cs)
  | v1 == v2 = solveLoop (n - 1) cs
  | otherwise = do
    bindVar x v1 (UVar v2)
    solveLoop (n - 1) =<< traverse zonkConstraint cs -- -}
solveLoop n (With x (UVar v :~ UTerm t) : cs) = do
  bindVar x v (UTerm t)
  solveLoop (n - 1) =<< traverse zonkConstraint cs
solveLoop n (With x (UTerm t :~ UVar v) : cs) = do
  bindVar x v (UTerm t)
  solveLoop (n - 1) =<< traverse zonkConstraint cs
solveLoop n (With x (UTerm t1 :~ UTerm t2) : cs) = do
  let cs' = unify x t1 t2
  solveLoop (n - 1) $ cs' <> cs

zonkConstraint :: (Applicative f, MonadBind t v f, Traversable t) => WithMeta x t v -> f (WithMeta x t v)
zonkConstraint (With m (x :~ y)) =
  With m <$> ((:~) <$> zonkUTerm x <*> zonkUTerm y)

zonkUTerm :: (MonadBind t v f, Traversable t, Applicative f) => UTerm t v -> f (UTerm t v)
zonkUTerm (UVar v) = do
  mterm <- lookupVar v
  mterm <- traverse zonkUTerm mterm
  pure $ fromMaybe (UVar v) mterm
zonkUTerm (UTerm t) = UTerm <$> traverse zonkUTerm t

errorWithMeta :: (HasCallStack, Pretty x) => x -> Doc -> a
errorWithMeta meta msg =
  errorDoc $
    "error:" $+$ nest 2 msg
      $$ "info:" <+> pPrint meta

unifyErrorMessage :: (Pretty a, Pretty b) => a -> b -> Doc
unifyErrorMessage t1 t2 = "Couldn't match" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

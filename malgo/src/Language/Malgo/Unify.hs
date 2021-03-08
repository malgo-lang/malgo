{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Unify where

import Control.Monad.Cont (ContT)
import Control.Monad.Identity (IdentityT)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Data.HashSet as HashSet
import Koriel.Pretty
import Language.Malgo.Prelude

----------------
-- Constraint --
----------------

infixl 5 :~

data Constraint t = t :~ t
  deriving stock (Eq, Ord, Show, Generic)

instance (Pretty t) => Pretty (Constraint t) where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

type WithMeta x t = With x (Constraint t)

---------------
-- Unifiable --
---------------

class (Hashable (Var t), Eq (Var t), Eq t, Pretty t) => Unifiable t where
  type Var t
  unify :: Pretty x => x -> t -> t -> (HashMap (Var t) t, [WithMeta x t])
  equiv :: t -> t -> Maybe (HashMap (Var t) (Var t))
  freevars :: t -> HashSet (Var t)
  occursCheck :: (Eq (Var t), Hashable (Var t)) => Var t -> t -> Bool
  occursCheck v t = HashSet.member v (freevars t)

class Unifiable1 t where
  liftUnify :: (Pretty x, Unifiable a) => (x -> a -> a -> (HashMap (Var a) a, [WithMeta x a])) -> x -> t a -> t a -> (HashMap (Var a) a, [WithMeta x a])
  liftEquiv :: Unifiable a => (a -> a -> Maybe (HashMap (Var a) (Var a))) -> t a -> t a -> Maybe (HashMap (Var a) (Var a))
  liftFreevars :: Unifiable a => (a -> HashSet (Var a)) -> t a -> HashSet (Var a)
  liftOccursCheck :: Unifiable a => (Var a -> a -> Bool) -> Var a -> t a -> Bool

class Monad m => MonadBind t m where
  lookupVar :: Var t -> m (Maybe t)
  default lookupVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => Var t -> m (Maybe t)
  lookupVar v = lift (lookupVar v)
  freshVar :: m (Var t)
  default freshVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => m (Var t)
  freshVar = lift (freshVar @t)
  newVar :: Pretty x => x -> t -> m (Var t)
  newVar x t = do
    v <- freshVar @t
    bindVar x v t
    pure v
  bindVar :: Pretty x => x -> Var t -> t -> m ()
  default bindVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1, Pretty x) => x -> Var t -> t -> m ()
  bindVar x v t = lift (bindVar x v t)
  zonk :: t -> m t
  default zonk :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => t -> m t
  zonk t = lift (zonk t)

instance MonadBind t m => MonadBind t (IdentityT m)

instance MonadBind t m => MonadBind t (ReaderT r m)

instance MonadBind t m => MonadBind t (ExceptT e m)

instance MonadBind t m => MonadBind t (StateT s m)

instance MonadBind t m => MonadBind t (Lazy.StateT s m)

instance MonadBind t m => MonadBind t (WriterT w m)

instance MonadBind t m => MonadBind t (ContT r m)

------------
-- Solver --
------------

solve ::
  ( Pretty x,
    MonadBind t m,
    Unifiable t
  ) =>
  [WithMeta x t] ->
  m ()
solve = solveLoop 5000

solveLoop ::
  ( Pretty x,
    MonadBind t m,
    Unifiable t
  ) =>
  Int ->
  [WithMeta x t] ->
  m ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (With x (t1 :~ t2) : cs) = do
  let (binds, cs') = unify x t1 t2
  ifor_ binds $ \var term -> bindVar x var term
  solveLoop (n - 1) =<< traverse zonkConstraint (cs' <> cs)

zonkConstraint :: (Applicative f, MonadBind t f) => WithMeta x t -> f (WithMeta x t)
zonkConstraint (With m (x :~ y)) =
  With m <$> ((:~) <$> zonk x <*> zonk y)

errorWithMeta :: Pretty x => x -> Doc -> a
errorWithMeta meta msg =
  errorDoc $ "error:" $+$ nest 2 msg $$ "info:" <+> pPrint meta

unifyErrorMessage :: (Pretty a, Pretty b) => a -> b -> Doc
unifyErrorMessage t1 t2 = "Couldn't match" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)


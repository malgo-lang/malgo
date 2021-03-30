{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Unification library
module Language.Malgo.Unify where

import qualified Data.HashSet as HashSet
import Koriel.Pretty
import Language.Malgo.Prelude
import Text.Megaparsec (SourcePos)

----------------
-- Constraint --
----------------

infixl 5 :~

-- | Constraint
-- a :~ b means 'a ~ b'
data Constraint t = t :~ t
  deriving stock (Eq, Ord, Show, Generic)

instance (Pretty t) => Pretty (Constraint t) where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

---------------
-- Unifiable --
---------------

-- | Unifiable value
class (Hashable (Var t), Eq (Var t), Eq t, Pretty t) => Unifiable t where
  -- | Representation of variable
  type Var t

  -- | Unify two terms and generate substituation and new constraints
  unify :: MonadMalgo m => SourcePos -> t -> t -> m (HashMap (Var t) t, [With SourcePos (Constraint t)])

  -- | Check alpha-equivalence
  equiv :: t -> t -> Maybe (HashMap (Var t) (Var t))

  -- | Free variables
  freevars :: t -> HashSet (Var t)

  -- | Occurs check
  occursCheck :: (Eq (Var t), Hashable (Var t)) => Var t -> t -> Bool
  occursCheck v t = HashSet.member v (freevars t)

-- | Lifted version of Unifiable
class Unifiable1 t where
  liftUnify :: (MonadMalgo m, Unifiable a) => (SourcePos -> a -> a -> m (HashMap (Var a) a, [With SourcePos (Constraint a)])) -> SourcePos -> t a -> t a -> m (HashMap (Var a) a, [With SourcePos (Constraint a)])
  liftEquiv :: Unifiable a => (a -> a -> Maybe (HashMap (Var a) (Var a))) -> t a -> t a -> Maybe (HashMap (Var a) (Var a))
  liftFreevars :: Unifiable a => (a -> HashSet (Var a)) -> t a -> HashSet (Var a)
  liftOccursCheck :: Unifiable a => (Var a -> a -> Bool) -> Var a -> t a -> Bool

class (MonadMalgo m, Unifiable t) => MonadBind t m where
  lookupVar :: Var t -> m (Maybe t)
  default lookupVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => Var t -> m (Maybe t)
  lookupVar v = lift (lookupVar v)
  freshVar :: m (Var t)
  default freshVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => m (Var t)
  freshVar = lift (freshVar @t)
  bindVar :: SourcePos -> Var t -> t -> m ()
  default bindVar :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => SourcePos -> Var t -> t -> m ()
  bindVar x v t = lift (bindVar x v t)

  -- Apply all substituation
  zonk :: t -> m t
  default zonk :: (MonadTrans tr, MonadBind t m1, m ~ tr m1) => t -> m t
  zonk t = lift (zonk t)

instance MonadBind t m => MonadBind t (ReaderT r m)

instance MonadBind t m => MonadBind t (ExceptT e m)

instance MonadBind t m => MonadBind t (StateT s m)

instance MonadBind t m => MonadBind t (WriterT w m)

------------
-- Solver --
------------

solve ::
  ( MonadBind t m,
    Unifiable t
  ) =>
  [With SourcePos (Constraint t)] ->
  m ()
solve = solveLoop 5000

solveLoop ::
  ( MonadBind t m,
    Unifiable t
  ) =>
  Int ->
  [With SourcePos (Constraint t)] ->
  m ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (With x (t1 :~ t2) : cs) = do
  (binds, cs') <- unify x t1 t2
  ifor_ binds $ \var term -> bindVar x var term
  solveLoop (n - 1) =<< traverse zonkConstraint (cs' <> cs)

zonkConstraint :: (MonadBind t f) => With x (Constraint t) -> f (With x (Constraint t))
zonkConstraint (With m (x :~ y)) = With m <$> ((:~) <$> zonk x <*> zonk y)

unifyErrorMessage :: (Pretty a, Pretty b) => a -> b -> Doc
unifyErrorMessage t1 t2 = "Couldn't match" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

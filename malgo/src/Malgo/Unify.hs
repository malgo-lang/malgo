{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Unification library
module Malgo.Unify where

import Control.Monad.Except (ExceptT)
import qualified Data.HashSet as HashSet
import Koriel.Pretty
import Malgo.Prelude
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

type UnifyResult t = (HashMap (Var t) t, [With SourcePos (Constraint t)])

unifyErrorMessage :: (Pretty a, Pretty b) => a -> b -> Doc
unifyErrorMessage t1 t2 = "Couldn't match" $$ nest 7 (pPrint t1) $$ nest 2 ("with" <+> pPrint t2)

type UnifyError = (SourcePos, Doc)

-- | Unifiable value
class (Hashable (Var t), Eq (Var t), Eq t, Pretty t) => Unifiable t where
  -- | Representation of variable
  type Var t

  -- | Unify two terms and generate substituation and new constraints
  unify :: SourcePos -> t -> t -> Either UnifyError (UnifyResult t)

  -- | Check alpha-equivalence
  equiv :: t -> t -> Maybe (HashMap (Var t) (Var t))

  -- | Free variables
  freevars :: t -> HashSet (Var t)

  -- | Occurs check
  occursCheck :: (Eq (Var t), Hashable (Var t)) => Var t -> t -> Bool
  occursCheck v t = HashSet.member v (freevars t)

-- | Lifted version of Unifiable
class Unifiable1 t where
  liftUnify :: (Unifiable a) => (SourcePos -> a -> a -> Either UnifyError (UnifyResult a)) -> SourcePos -> t a -> t a -> Either UnifyError (UnifyResult a)
  liftEquiv :: Unifiable a => (a -> a -> Maybe (HashMap (Var a) (Var a))) -> t a -> t a -> Maybe (HashMap (Var a) (Var a))
  liftFreevars :: Unifiable a => (a -> HashSet (Var a)) -> t a -> HashSet (Var a)
  liftOccursCheck :: Unifiable a => (Var a -> a -> Bool) -> Var a -> t a -> Bool

class (Monad m, Unifiable t) => MonadBind t m where
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

instance (Monoid w, MonadBind t m) => MonadBind t (WriterT w m)

------------
-- Solver --
------------

solve ::
  (MonadBind t m, MonadReader env m, MonadIO m, HasOpt env) =>
  [With SourcePos (Constraint t)] ->
  m ()
solve = solveLoop 5000

solveLoop ::
  (MonadBind t m, MonadReader env m, MonadIO m, HasOpt env) =>
  Int ->
  [With SourcePos (Constraint t)] ->
  m ()
solveLoop n _ | n <= 0 = error "Constraint solver error: iteration limit"
solveLoop _ [] = pure ()
solveLoop n (With x (t1 :~ t2) : cs) = do
  (binds, cs') <- case unify x t1 t2 of
    Right x -> pure x
    Left (pos, err) -> errorOn pos err
  ifor_ binds $ \var term -> bindVar x var term
  solveLoop (n - 1) =<< traverse zonkConstraint (cs' <> cs)

zonkConstraint :: (MonadBind t f) => With x (Constraint t) -> f (With x (Constraint t))
zonkConstraint (With m (x :~ y)) = With m <$> ((:~) <$> zonk x <*> zonk y)

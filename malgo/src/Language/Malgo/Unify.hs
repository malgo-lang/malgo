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
import qualified Data.Set as Set
import GHC.Generics (Generic1)
import Koriel.Pretty
import Language.Malgo.Prelude

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

freeze :: Traversable t => UTerm t v -> Maybe (Fix t)
freeze (UVar _) = Nothing
freeze (UTerm t) = Fix <$> traverse freeze t

unfreeze :: Functor t => Fix t -> UTerm t v
unfreeze = UTerm . fmap unfreeze . unFix

freevars :: (Ord a, Foldable t) => UTerm t a -> Set a
freevars (UVar v) = Set.singleton v
freevars (UTerm t) = foldMap freevars t

occursCheck :: (Ord a, Foldable t, Pretty x, Pretty a, Pretty1 t, Applicative f) => x -> a -> UTerm t a -> f ()
occursCheck x v t =
  if Set.member v (freevars t)
    then errorWithMeta x $ "Occurs check:" <+> quotes (pPrint v) <+> "for" <+> pPrint t
    else pure ()

class HasUTerm t v a where
  walkOn :: Traversal' a (UTerm t v)

instance HasUTerm t v (UTerm t v) where
  walkOn = id

data UScheme t v = UScheme [Fix t] (UTerm t v)
  deriving stock (Eq, Ord, Show, Generic)

instance (Pretty v, Pretty1 t) => Pretty (UScheme t v) where
  pPrintPrec l _ (UScheme as t) = "forall" <+> sep (map (pPrintPrec l 0) as) <> "." <+> pPrintPrec l 0 t

instance (Traversable t, HasUTerm t v (UTerm t v)) => HasUTerm t v (UScheme t v) where
  walkOn f (UScheme vs t) = UScheme <$> traverse (walkOn' f) vs <*> f t
    where
      walkOn' f v = fromMaybe v . freeze <$> walkOn f (unfreeze v `asTypeOf` t)

----------------
-- Constraint --
----------------

infixl 5 :~

data Constraint t v = UTerm t v :~ UTerm t v
  deriving stock (Eq, Ord, Show, Generic)

instance (Pretty v, Pretty1 t) => Pretty (Constraint t v) where
  pPrint (t1 :~ t2) = pPrint t1 <+> "~" <+> pPrint t2

data WithMeta x t v = WithMeta x (Constraint t v)
  deriving stock (Eq, Ord, Show, Generic)

instance (Pretty v, Pretty1 t) => Pretty (WithMeta x t v) where
  pPrint (WithMeta _ c) = pPrint c

--------------
-- Variable --
--------------

class Var t v | v -> t, t -> v where
  rigidName :: Lens' v String
  isRigid :: v -> Bool
  isRigid v = not $ null $ v ^. rigidName
  toRigidName :: Fix t -> String
  toBound :: MonadBind t v m => v -> String -> m (Fix t)

---------------
-- Unifiable --
---------------

class Unifiable t v | t -> v where
  unify :: Pretty x => x -> t (UTerm t v) -> t (UTerm t v) -> [WithMeta x t v]

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

generalizeUTerm ::
  (Pretty x, Ord v, Foldable t, Var t v, MonadBind t v m, Traversable t) =>
  x ->
  -- | bounded variables
  Set v ->
  UTerm t v ->
  m (UScheme t v)
generalizeUTerm x bound term = do
  -- {-
  let fvs = Set.toList $ unboundFreevars bound term
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ unfreeze a) fvs as
  UScheme as <$> zonkUTerm term

-- -}
{-
  zonkedTerm <- zonkUTerm term
  let fvs = Set.toList $ unboundFreevars bound zonkedTerm
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ unfreeze a) fvs as
  UScheme as <$> zonkUTerm zonkedTerm

-- -}

unboundFreevars :: (Ord v, Foldable t) => Set v -> UTerm t v -> Set v
unboundFreevars bound t = freevars t Set.\\ bound

generalizeUTermMutRecs :: (Pretty x, Ord v, Foldable t, Var t v, MonadBind t v m, Traversable t, Pretty v, Pretty1 t) => x -> Set v -> [UTerm t v] -> m ([Fix t], [UScheme t v])
generalizeUTermMutRecs x bound terms = do
  -- {-
  let fvs = Set.toList $ mconcat $ map (unboundFreevars bound) terms
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ unfreeze a) fvs as
  (as,) <$> traverse (fmap (UScheme as) . zonkUTerm) terms

-- -}
{-
  zonkedTerms <- traverse zonkUTerm terms
  let fvs = Set.toList $ mconcat $ map (unboundFreevars bound) zonkedTerms
  as <- zipWithM toBound fvs [[c] | c <- ['a' ..]]
  zipWithM_ (\fv a -> bindVar x fv $ unfreeze a) fvs as
  (as,) <$> traverse (fmap (UScheme as) . zonkUTerm) zonkedTerms

-- -}

instantiateUTerm :: (Pretty x, Var t v, MonadBind t v m, Eq v, Eq1 t, Traversable t) => x -> Bool -> UScheme t v -> m (UTerm t v)
instantiateUTerm _ isRigid (UScheme as t) = do
  vs <- traverse ?? as $ \a -> do
    v <- freshVar
    if isRigid
      then pure $ UVar $ v & rigidName .~ toRigidName a
      else pure $ UVar v
  replace (zip as vs) t
  where
    replace [] t = pure t
    replace ((a, v) : avs) t
      | unfreeze a == t = replace avs v
      | otherwise = case t of
        UVar _ -> replace avs t
        UTerm t' -> replace avs . UTerm =<< traverse (replace [(a, v)]) t'

------------
-- Solver --
------------

solve ::
  ( Var t v,
    Pretty x,
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
  ( Var t v,
    Pretty x,
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
solveLoop n (WithMeta x (UVar v1 :~ UVar v2) : cs)
  | v1 == v2 = solveLoop (n - 1) cs
  | isRigid v1 && isRigid v2 && v1 ^. rigidName /= v2 ^. rigidName =
    errorWithMeta x $
      unifyErrorMessage v1 v2
        $+$ quotes (pPrint v1) <+> "and" <+> quotes (pPrint v2) <+> "are rigid variable"
  | isRigid v1 = do
    bindVar x v2 (UVar v1)
    solveLoop (n - 1) =<< traverse zonkConstraint cs
  | isRigid v2 = do
    bindVar x v1 (UVar v2)
    solveLoop (n - 1) =<< traverse zonkConstraint cs
  {-  | otherwise = errorWithMeta x $ unifyErrorMessage v1 v2 -- -}
  | otherwise = do
    bindVar x v1 (UVar v2)
    solveLoop (n - 1) =<< traverse zonkConstraint cs -- -}
solveLoop n (WithMeta x (UVar v :~ UTerm t) : cs)
  | isRigid v = errorWithMeta x $ unifyErrorMessage v (UTerm t) $+$ quotes (pPrint v) <+> "is a rigid variable"
  | otherwise = do
    bindVar x v (UTerm t)
    solveLoop (n - 1) =<< traverse zonkConstraint cs
solveLoop n (WithMeta x (UTerm t :~ UVar v) : cs)
  | isRigid v = errorWithMeta x $ unifyErrorMessage v (UTerm t) $+$ quotes (pPrint v) <+> "is a rigid variable"
  | otherwise = do
    bindVar x v (UTerm t)
    solveLoop (n - 1) =<< traverse zonkConstraint cs
solveLoop n (WithMeta x (UTerm t1 :~ UTerm t2) : cs) = do
  let cs' = unify x t1 t2
  solveLoop (n - 1) $ cs' <> cs

zonkConstraint :: (Applicative f, MonadBind t v f, Traversable t) => WithMeta x t v -> f (WithMeta x t v)
zonkConstraint (WithMeta m (x :~ y)) =
  WithMeta m <$> ((:~) <$> zonkUTerm x <*> zonkUTerm y)

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

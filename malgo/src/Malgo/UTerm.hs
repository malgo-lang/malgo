{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.UTerm where

import Data.Fix
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare), Show1 (liftShowsPrec))
import Data.Void
import Koriel.Pretty
import Malgo.Prelude
import Text.Show (Show (showList, showsPrec), showParen, showString)
import qualified RIO.HashSet as HashSet

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

deriving stock instance Generic (UTerm t v)

instance (Pretty v, Pretty (t (UTerm t v))) => Pretty (UTerm t v) where
  pPrintPrec _ _ (UVar v) = pPrint v
  pPrintPrec l d (UTerm t) = pPrintPrec l d t

freeze :: Traversable t => UTerm t v -> Maybe (Fix t)
freeze (UVar _) = Nothing
freeze (UTerm t) = Fix <$> traverse freeze t

unfreeze :: Functor t => Fix t -> UTerm t v
unfreeze = UTerm . fmap unfreeze . unFix

freevars :: (Hashable a, Foldable t, Eq a) => UTerm t a -> HashSet a
freevars (UVar v) = HashSet.singleton v
freevars (UTerm t) = foldMap freevars t

class HasUTerm t v a where
  walkOn :: Monad f => (UTerm t v -> f (UTerm t v)) -> a -> f a

instance HasUTerm t v (UTerm t v) where
  walkOn = id

instance HasUTerm t v x => HasUTerm t v (With x a) where
  walkOn f (With x a) = With <$> walkOn f x <*> pure a

instance HasUTerm t v Void where
  walkOn _ x = absurd x


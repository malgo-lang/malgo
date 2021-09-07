{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Infer.UTerm where

import Control.Lens (Plated)
import Data.Data (Data)
import Data.Fix
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare), Show1 (liftShowsPrec))
import qualified Data.HashSet as HashSet
import Koriel.Pretty
import Malgo.Prelude
import Text.Show (Show (showList, showsPrec), showParen, showString)

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

deriving stock instance (Data v, Data (t (UTerm t v)), Typeable t) => Data (UTerm t v)

instance (Data v, Data (t (UTerm t v)), Typeable t) => Plated (UTerm t v)

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

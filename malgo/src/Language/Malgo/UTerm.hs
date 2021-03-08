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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.UTerm where

import Data.Fix
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare), Show1 (liftShowsPrec))
import qualified Data.HashSet as HashSet
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

class HasUTerm t v a where
  walkOn :: Traversal' a (UTerm t v)

instance (Traversable t, HasUTerm t v v) => HasUTerm t v (UTerm t v) where
  walkOn = id

instance HasUTerm t v x => HasUTerm t v (With x a) where
  walkOn f (With x a) = With <$> walkOn f x <*> pure a

instance HasUTerm t v Void where
  walkOn _ x = absurd x

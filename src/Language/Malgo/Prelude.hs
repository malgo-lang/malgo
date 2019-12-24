{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Prelude
  ( module Relude
  , module Relude.Extra.Map
  , foldFor
  , foldForA
  , foldForM
  )
where

import           Relude                  hiding ( Type )
import           Relude.Extra.Map        hiding ( size )

foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap

foldForA :: (Monoid b, Applicative m, Foldable f) => f a -> (a -> m b) -> m b
foldForA = flip foldMapA

foldForM :: (Monoid b, Monad m, Foldable f) => f a -> (a -> m b) -> m b
foldForM = flip foldMapM

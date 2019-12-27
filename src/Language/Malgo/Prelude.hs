{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Language.Malgo.Prelude
  ( module Relude
  , module Relude.Extra.Map
  , foldFor
  , foldForA
  , foldForM
  , Complement(..)
  )
where

import           Relude                  hiding ( Constraint
                                                , Type
                                                , Op
                                                , init
                                                )
import           Relude.Extra.Map        hiding ( size
                                                , delete
                                                )
import qualified Data.Set                      as Set
import qualified Data.List                     as List

foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap

foldForA :: (Monoid b, Applicative m, Foldable f) => f a -> (a -> m b) -> m b
foldForA = flip foldMapA

foldForM :: (Monoid b, Monad m, Foldable f) => f a -> (a -> m b) -> m b
foldForM = flip foldMapM

class One a => Complement a where
  (\\) :: a -> a -> a
  delete :: OneItem a -> a -> a
  delete x s = s \\ one x

instance Ord a => Complement (Set a) where
  (\\) = (Set.\\)

instance Eq a => Complement [a] where
  (\\) = (List.\\)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Language.Malgo.Prelude
  ( module Relude
  , module Relude.Extra.Map
  , foldFor
  , foldForA
  , foldForM
  , Complement(..)
  , localState
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

{-# INLINE foldFor #-}
foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap

{-# INLINE foldForA #-}
foldForA :: (Monoid b, Applicative m, Foldable f) => f a -> (a -> m b) -> m b
foldForA = flip foldMapA

{-# INLINE foldForM #-}
foldForM :: (Monoid b, Monad m, Foldable f) => f a -> (a -> m b) -> m b
foldForM = flip foldMapM

class One a => Complement a where
  (\\) :: a -> a -> a

  {-# INLINE delete #-}
  delete :: OneItem a -> a -> a
  delete x s = s \\ one x

instance Ord a => Complement (Set a) where
  (\\) = (Set.\\)

instance Eq a => Complement [a] where
  (\\) = (List.\\)

{-# INLINE localState #-}
localState :: MonadState s m => s -> m a -> m a
localState s m = do
  backup <- get
  put s
  v <- m
  put backup
  pure v
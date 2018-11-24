{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
module Language.Malgo.Prelude
  ( module U
  , NaturalTransformation
  , type (~>)
  , HeytingAlgebra(..)
  ) where

import qualified Prelude   as P
import           Universum as U hiding (Type, not)

-- natural transformation
type NaturalTransformation f g = forall a. f a -> g a
type f ~> g = NaturalTransformation f g

-- heyting algebra
class HeytingAlgebra a where
  ff :: a
  tt :: a
  implies :: a -> a -> a
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

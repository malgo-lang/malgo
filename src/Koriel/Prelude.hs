{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( module Relude,
    unzip,
    replaceOf,
    localState,
  )
where

import Control.Lens (ASetter, over)
import Data.Monoid
import Relude hiding (Op, Type, unzip, All, id)

-- | unzip :: [(a, b)] -> ([a], [b]) の一般化
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
{-# INLINE unzip #-}

replaceOf :: Eq b => ASetter s t b b -> b -> b -> s -> t
replaceOf l x x' = over l (\v -> if v == x then x' else v)

localState :: MonadState s m => m a -> m (a, s)
localState action = do
  backup <- get
  result <- action
  state <- get
  put backup
  pure (result, state)
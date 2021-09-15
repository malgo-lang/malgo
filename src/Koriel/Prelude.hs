{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( module Relude,
    module Control.Monad.Writer.Class,
    module Control.Monad.Trans.Writer.CPS,
    unzip,
    replaceOf,
    localState,
  )
where

import Control.Lens (ASetter, over)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import qualified Control.Monad.Trans.Writer.CPS as W
import Control.Monad.Writer.Class hiding (pass)
import qualified Control.Monad.Writer.Class as Writer
import Data.Monoid
import Relude hiding (All, Op, Type, id, unzip)

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

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  tell = W.tell
  listen = W.listen
  pass = W.pass

instance MonadState s m => MonadState s (WriterT w m) where
  state = lift . state

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  reader = lift . reader
  local = W.mapWriterT . local
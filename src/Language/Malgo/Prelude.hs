{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Malgo.Prelude
  ( module Relude
  , module Relude.Extra.Map
  , module Control.Monad.Trans.Writer.CPS
  , module Control.Monad.Writer.Class
  , foldFor
  , foldForA
  , foldForM
  , Complement(..)
  , localState
  , unzip
  )
where

import           Relude                  hiding ( Constraint
                                                , Type
                                                , Op
                                                , init
                                                , unzip
                                                , pass
                                                )
import           Relude.Extra.Map        hiding ( size
                                                , delete
                                                )
import qualified Data.Set                      as Set
import qualified Data.List                     as List
import           Control.Monad.Trans.Writer.CPS ( WriterT, runWriterT )
import qualified Control.Monad.Trans.Writer.CPS as W
import           Control.Monad.Writer.Class

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

unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer = W.writer
  tell = W.tell
  listen = W.listen
  pass = W.pass

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask = lift ask
  local = W.mapWriterT . local
  reader = lift . reader

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get = lift get
  put = lift . put
  state = lift . state
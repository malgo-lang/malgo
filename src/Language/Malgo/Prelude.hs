{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Malgo.Prelude
  ( module Relude
  , module Control.Monad.Trans.Writer.CPS
  , module Control.Monad.Writer.Class
  , unzip
  , ltraverse
  , rtraverse
  , Unreachable(..)
  )
where

import           Relude                  hiding ( Constraint
                                                , Type
                                                , Op
                                                , unzip
                                                , pass
                                                , return
                                                )
import           Control.Monad.Trans.Writer.CPS ( WriterT
                                                , runWriterT
                                                )
import qualified Control.Monad.Trans.Writer.CPS
                                               as W
import           Control.Monad.Writer.Class
import           Text.PrettyPrint.HughesPJClass ( Pretty(..)
                                                , text
                                                )
import           Text.Parsec.Pos                ( SourcePos )

-- unzip :: [(a, b)] -> ([a], [b]) の一般化
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
{-# INLINE unzip #-}

-- bitraverseの部分適用
ltraverse :: (Bitraversable t, Applicative f) => (a -> f c) -> t a d -> f (t c d)
ltraverse f = bitraverse f pure
{-# INLINE ltraverse #-}

rtraverse :: (Bitraversable t, Applicative f) => (b -> f c) -> t a b -> f (t a c)
rtraverse = bitraverse pure
{-# INLINE rtraverse #-}

-- mtlのインスタンスの追加定義
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer = W.writer
  tell   = W.tell
  listen = W.listen
  pass   = W.pass
  {-# INLINE writer #-}
  {-# INLINE tell #-}
  {-# INLINE listen #-}
  {-# INLINE pass #-}

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask    = lift ask
  local  = W.mapWriterT . local
  reader = lift . reader
  {-# INLINE ask #-}
  {-# INLINE local #-}
  {-# INLINE reader #-}

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get   = lift get
  put   = lift . put
  state = lift . state
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

-- Unreachable
data Unreachable = Unreachable
  deriving stock (Show, Typeable)

instance Exception Unreachable

-- Pretty SourcePos
instance Pretty SourcePos where
  pPrint = text . show

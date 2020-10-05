{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( module Prelude,
    module Control.Applicative,
    module Optics,
    module Control.Monad,
    module Control.Monad.IO.Class,
    module Control.Monad.Reader.Class,
    module Control.Monad.State.Class,
    module Control.Monad.Trans,
    module Control.Monad.Trans.Except,
    module Control.Monad.Trans.Reader,
    module Control.Monad.Trans.State.Strict,
    module Control.Monad.Trans.Writer.CPS,
    module Control.Monad.Writer.Class,
    module Data.Bifunctor,
    module Data.Bitraversable,
    module Data.Coerce,
    module Data.Foldable,
    module Data.List.NonEmpty,
    module Data.Maybe,
    module Data.Map,
    module Data.Monoid,
    module Data.Set,
    module Data.String,
    module Data.Text,
    module GHC.Stack,
    unzip,
    asumMap,
    foldMapA,
    ifoldMapA,
    (<<$>>),
    (??),
    ordNub,
    replaceOf,
    IORef,
    newIORef,
    readIORef,
    writeIORef,
    Bug,
    bug,
    localState,
    Unreachable (..),
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
    runReader,
    runReaderT,
  )
import Control.Monad.Trans.State.Strict
  ( StateT (..),
    evalStateT,
    execStateT,
    runStateT,
  )
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import qualified Control.Monad.Trans.Writer.CPS as W
import Control.Monad.Writer.Class
import Data.Bifunctor
import Data.Bitraversable
import Data.Coerce
import Data.Foldable
import Data.IORef (IORef)
import qualified Data.IORef as I
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Typeable
import GHC.Stack
  ( CallStack,
    HasCallStack,
    callStack,
    prettyCallStack,
  )
import Optics
import qualified Text.Megaparsec.Pos as Megaparsec
import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint.HughesPJClass (Pretty (..), text)
import Prelude hiding (log, unzip)

-- | unzip :: [(a, b)] -> ([a], [b]) の一般化
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
{-# INLINE unzip #-}

-- | Alternative version of @asum@.
asumMap :: forall b m f a. (Foldable f, Alternative m) => (a -> m b) -> f a -> m b
asumMap = coerce (foldMap :: (a -> Alt m b) -> f a -> Alt m b)
{-# INLINE asumMap #-}

foldMapA :: forall b m f a. (Semigroup b, Monoid b, Applicative m, Foldable f) => (a -> m b) -> f a -> m b
foldMapA = coerce (foldMap :: (a -> Ap m b) -> f a -> Ap m b)
{-# INLINE foldMapA #-}

ifoldMapA :: forall i b m f a. (Semigroup b, Monoid b, Applicative m, FoldableWithIndex i f) => (i -> a -> m b) -> f a -> m b
ifoldMapA = coerce (ifoldMap :: (i -> a -> Ap m b) -> f a -> Ap m b)
{-# INLINE ifoldMapA #-}

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
{-# INLINE (<<$>>) #-}

infixl 1 ??

(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab

ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x : xs)
      | x `Set.member` s = go s xs
      | otherwise = x : go (Set.insert x s) xs

replaceOf :: (Is k A_Setter, Eq b) => Optic k is s t b b -> b -> b -> s -> t
replaceOf l x x' = over l (\v -> if v == x then x' else v)

newIORef :: MonadIO m => a -> m (IORef a)
newIORef a = liftIO $ I.newIORef a

readIORef :: MonadIO m => IORef a -> m a
readIORef r = liftIO $ I.readIORef r

writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef r v = liftIO $ I.writeIORef r v

data Bug = Bug SomeException CallStack
  deriving stock (Show)

instance Exception Bug where
  displayException (Bug e cStack) =
    displayException e ++ "\n"
      ++ prettyCallStack cStack

bug :: (HasCallStack, Exception e) => e -> a
bug e = throw $ toException (Bug (toException e) callStack)

localState :: MonadState s m => m a -> m (a, s)
localState action = do
  backup <- get
  result <- action
  state <- get
  put backup
  pure (result, state)

-- mtlのインスタンスの追加定義
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  writer = W.writer
  tell = W.tell
  listen = W.listen
  pass = W.pass
  {-# INLINE writer #-}
  {-# INLINE tell #-}
  {-# INLINE listen #-}
  {-# INLINE pass #-}

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask = lift ask
  local = W.mapWriterT . local
  reader = lift . reader
  {-# INLINE ask #-}
  {-# INLINE local #-}
  {-# INLINE reader #-}

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get = lift get
  put = lift . put
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

instance Pretty Megaparsec.SourcePos where
  pPrint = text . Megaparsec.sourcePosPretty
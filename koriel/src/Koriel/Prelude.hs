{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( module Prelude,
    module Control.Applicative,
    module Control.Lens,
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
    module Data.HashMap.Strict,
    module Data.HashSet,
    module Data.Hashable,
    module Data.List.NonEmpty,
    module Data.Monoid,
    module Data.String,
    module Data.Text,
    module Data.Traversable,
    module GHC.Generics,
    module GHC.Stack,
    module Witherable,
    unzip,
    asumMap,
    foldMapA,
    ifoldMapA,
    (<<$>>),
    replaceOf,
    IORef,
    newIORef,
    readIORef,
    writeIORef,
    Bug (..),
    bug,
    undefined,
    localState,
    Unreachable (..),
    modifyIORef,
  )
where

import Control.Applicative
import Control.Exception
import Control.Lens
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
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.IORef (IORef)
import qualified Data.IORef as I
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Traversable
import Data.Typeable
import GHC.Exts (RuntimeRep, TYPE)
import GHC.Generics (Generic)
import GHC.Stack
  ( CallStack,
    HasCallStack,
    callStack,
    prettyCallStack,
  )
import qualified Text.Megaparsec.Pos as Megaparsec
import Text.PrettyPrint.HughesPJClass (Pretty (..), text)
import Witherable
import Prelude hiding (filter, log, undefined, unzip)
import qualified Prelude

-- | unzip :: [(a, b)] -> ([a], [b]) の一般化
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
{-# INLINE unzip #-}

-- | Alternative version of @asum@.
asumMap :: forall b m f a. (Foldable f, Alternative m) => (a -> m b) -> f a -> m b
asumMap = coerce (foldMap :: (a -> Alt m b) -> f a -> Alt m b)
{-# INLINE asumMap #-}

foldMapA :: forall b m f a. (Monoid b, Applicative m, Foldable f) => (a -> m b) -> f a -> m b
foldMapA = coerce (foldMap :: (a -> Ap m b) -> f a -> Ap m b)
{-# INLINE foldMapA #-}

ifoldMapA :: forall b m a. (Monoid b, Applicative m) => (Int -> a -> m b) -> [a] -> m b
ifoldMapA = coerce (ifoldMap :: (Int -> a -> Ap m b) -> [a] -> Ap m b)
{-# INLINE ifoldMapA #-}

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
{-# INLINE (<<$>>) #-}

replaceOf :: Eq b => ASetter s t b b -> b -> b -> s -> t
replaceOf l x x' = over l (\v -> if v == x then x' else v)

newIORef :: MonadIO m => a -> m (IORef a)
newIORef a = liftIO $ I.newIORef a

readIORef :: MonadIO m => IORef a -> m a
readIORef r = liftIO $ I.readIORef r

modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef r f = liftIO $ I.modifyIORef r f

writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef r v = liftIO $ I.writeIORef r v

-- Unreachable
data Unreachable = Unreachable
  deriving stock (Show, Typeable)

instance Exception Unreachable

data Bug = Bug SomeException CallStack
  deriving stock (Show)

instance Exception Bug where
  displayException (Bug e cStack) =
    "internal error [" <> displayException e <> "]\n"
      <> prettyCallStack cStack

bug :: (HasCallStack, Exception e) => e -> a
bug e = throw $ toException (Bug (toException e) callStack)

localState :: MonadState s m => m a -> m (a, s)
localState action = do
  backup <- get
  result <- action
  state <- get
  put backup
  pure (result, state)

undefined :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => a
undefined = Prelude.undefined
{-# WARNING undefined "'undefined' function remains in code" #-}

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

instance MonadState s m => MonadState s (WriterT w m) where
  get = lift get
  put = lift . put
  state = lift . state
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

-- Pretty SourcePos
instance Pretty Megaparsec.SourcePos where
  pPrint = text . Megaparsec.sourcePosPretty
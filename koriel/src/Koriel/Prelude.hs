{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( module RIO,
    module RIO.State,
    module RIO.Writer,
    module Control.Lens,
    module Witherable,
    unzip,
    asumMap,
    foldMapA,
    ifoldMapA,
    (<<$>>),
    replaceOf,
    Bug (..),
    bug,
    localState,
    Unreachable (..),
  )
where

import Control.Exception (throw)
import Control.Lens hiding (List)
import Data.Coerce (coerce)
import Data.Monoid
import GHC.Stack (callStack, prettyCallStack)
import RIO hiding (ASetter, ASetter', Getting, Lens, Lens', catMaybes, filter, lens, mapMaybe, over, preview, set, sets, to, view, (%~), (.~), (^.), (^..), (^?))
import RIO.State
import RIO.Writer
import qualified Text.Megaparsec.Pos as Megaparsec
import Text.PrettyPrint.HughesPJClass (Pretty (..), text)
import Witherable
import Prelude hiding (filter, log, undefined, unzip)

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

-- Pretty SourcePos
instance Pretty Megaparsec.SourcePos where
  pPrint = text . Megaparsec.sourcePosPretty
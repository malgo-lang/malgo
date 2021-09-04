{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( module RIO,
    module RIO.State, module RIO.Writer,
    module Control.Lens,
    module Witherable,
    unzip,
    replaceOf,
    localState,
  )
where

import Control.Lens hiding (List)
import Data.Monoid
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

replaceOf :: Eq b => ASetter s t b b -> b -> b -> s -> t
replaceOf l x x' = over l (\v -> if v == x then x' else v)

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
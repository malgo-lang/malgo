{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( -- * Reexports
    module Control.Arrow,
    module Control.Lens,
    module Control.Monad,
    module Control.Monad.IO.Class,
    module Control.Monad.Reader,
    module Control.Monad.Reader.Class,
    module Control.Monad.State.Class,
    module Control.Monad.State.Strict,
    module Control.Monad.Trans.Class,
    module Control.Monad.Trans.Writer.CPS,
    module Control.Monad.Writer.Class,
    module Data.Bifunctor,
    module Data.Bitraversable,
    module Data.Coerce,
    module Data.Foldable,
    module Data.Foldable.Extra,
    module Data.Functor,
    module Data.Function,
    module Data.HashMap.Strict,
    module Data.HashSet,
    module Data.Hashable,
    module Data.IORef,
    module Data.List.NonEmpty,
    module Data.String.Conversions,
    module Data.Text,
    module Data.Typeable,
    module Data.Void,
    module GHC.Generics,
    module GHC.Stack,
    module Prelude,
    module System.Exit,
    module System.IO,
    module Witherable,

    -- * Utilities
    unzip,
    replaceOf,
    localState,
    chomp,
    PrettyShow (..),
    identity,
    pass,

    -- * Lift IO functions

    -- ** Show
    hPrint,

    -- ** String
    hPutStr,
    hPutStrLn,

    -- ** Text
    hPutText,
    hPutTextLn,
    foldMapM,
  )
where

import Control.Arrow ((>>>))
import Control.Lens (ASetter, over, (??))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (..), runReader)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.State.Strict (StateT (..), evalState, evalStateT, execState, execStateT, runState)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Trans.Writer.CPS qualified as W
import Control.Monad.Writer.Class hiding (pass)
import Control.Monad.Writer.Class qualified as Writer
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Coerce (coerce)
import Data.Foldable
import Data.Foldable.Extra (allM, andM, anyM, orM)
import Data.Function
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable (..))
import Data.IORef
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Conversions
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Error.Diagnose.Compat.Megaparsec (HasHints (hints))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Exit
import System.IO (Handle, stderr, stdin, stdout)
import System.IO qualified
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Text.PrettyPrint.HughesPJClass qualified as P
import Witherable
import Prelude hiding (filter, id, unzip)

-- | Identity function
identity :: a -> a
identity x = x

-- | Generalization of 'Data.List.unzip' :: [(a, b)] -> ([a], [b])
unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
{-# INLINE unzip #-}

replaceOf :: (Eq b) => ASetter s t b b -> b -> b -> s -> t
replaceOf l x x' = over l (\v -> if v == x then x' else v)

localState :: (MonadState s m) => m a -> m (a, s)
localState action = do
  backup <- get
  result <- action
  state <- get
  put backup
  pure (result, state)

chomp :: String -> String
chomp = dropWhileEnd (`elem` ['\r', '\n'])

newtype PrettyShow a = PrettyShow a

instance (Show a) => Pretty (PrettyShow a) where
  pPrint (PrettyShow a) = P.text $ show a

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  tell = W.tell
  listen = W.listen
  pass = W.pass

instance (MonadState s m) => MonadState s (WriterT w m) where
  state = lift . state

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  reader = lift . reader
  local = W.mapWriterT . local

pass :: (Applicative m) => m ()
pass = pure ()

-- | foldMap for monadic functions
-- Based on rio package https://github.com/commercialhaskell/rio/pull/99#issuecomment-394179757
foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM (\acc x -> mappend acc <$> f x) mempty
{-# INLINE foldMapM #-}

-- Lift IO funcitons

-- | Lifted version of 'System.IO.hPrint'.
hPrint :: (MonadIO m, Show a) => Handle -> a -> m ()
hPrint handle x = liftIO $ System.IO.hPrint handle x

-- | Lifted version of 'System.IO.hPutStr'.
hPutStr :: (MonadIO m) => Handle -> String -> m ()
hPutStr handle x = liftIO $ System.IO.hPutStr handle x

-- | Lifted version of 'System.IO.hPutStrLn'.
hPutStrLn :: (MonadIO m) => Handle -> String -> m ()
hPutStrLn handle x = liftIO $ System.IO.hPutStrLn handle x

-- | Lifted version of 'T.hPutStr'.
hPutText :: (MonadIO m) => Handle -> Text -> m ()
hPutText handle x = liftIO $ T.hPutStr handle x

-- | Lifted version of 'T.hPutStrLn'.
hPutTextLn :: (MonadIO m) => Handle -> Text -> m ()
hPutTextLn handle x = liftIO $ T.hPutStrLn handle x

instance HasHints Void Text where
  hints = const []

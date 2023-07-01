{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( -- * Reexports
    module Control.Arrow,
    module Control.Lens,
    module Control.Monad,
    module Control.Monad.Extra,
    module Control.Monad.Error.Class,
    module Control.Monad.Except,
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
    module Data.ByteString.Short,
    module Data.Char,
    module Data.Coerce,
    module Data.Data,
    module Data.Either,
    module Data.Foldable,
    module Data.Foldable.Extra,
    module Data.Function,
    module Data.Functor,
    module Data.HashMap.Strict,
    module Data.HashSet,
    module Data.Hashable,
    module Data.Int,
    module Data.Kind,
    module Data.List,
    module Data.List.NonEmpty,
    module Data.Map.Strict,
    module Data.Maybe,
    module Data.Semigroup,
    module Data.String,
    module Data.String.Conversions,
    module Data.Text,
    module Data.Void,
    module GHC.Exts,
    module GHC.Generics,
    module GHC.Stack,
    module Prelude,
    module System.IO,
    IORef,

    -- * Utilities
    identity,
    pass,
    foldMapM,
    unzip,
    replaceOf,
    localState,
    chomp,
    asumMap,
    PrettyShow (..),

    -- * Lift IO functions

    -- ** Show
    hPrint,

    -- ** String
    hPutStr,
    hPutStrLn,

    -- ** Text
    hPutText,
    hPutTextLn,

    -- ** IORef
    newIORef,
    readIORef,
    modifyIORef,
    writeIORef,

    -- * Container utilities
    Container (..),
    one,
  )
where

import Control.Applicative
import Control.Arrow ((<<<), (>>>))
import Control.Lens (ASetter, over, (??))
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.State.Strict (StateT, evalState, evalStateT, execState, execStateT, runState, runStateT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, writerT)
import Control.Monad.Writer.Class hiding (pass)
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString.Short (ShortByteString)
import Data.Char
import Data.Coerce
import Data.Data (Typeable)
import Data.Either
import Data.Foldable
import Data.Foldable.Extra
import Data.Function (applyWhen, fix, on, (&))
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Int (Int32, Int64)
import Data.Kind (Constraint)
import Data.List (dropWhileEnd, foldl', sort, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Alt (..))
import Data.Semigroup
import Data.String
import Data.String.Conversions
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Void
import Error.Diagnose.Compat.Megaparsec (HasHints (hints))
import GHC.Exts (sortWith)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.IO (Handle, stderr, stdin, stdout)
import System.IO qualified
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Text.PrettyPrint.HughesPJClass qualified as P
import Prelude hiding (id, unzip)

identity :: a -> a
identity x = x

pass :: (Applicative f) => f ()
pass = pure ()

-- | @foldMapM@ from rio
foldMapM :: (Foldable t, Monad m, Monoid w) => (a -> m w) -> t a -> m w
foldMapM f =
  foldlM
    ( \acc a -> do
        w <- f a
        pure $! mappend acc w
    )
    mempty

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

asumMap :: forall b m f a. (Foldable f, Alternative m) => (a -> m b) -> f a -> m b
asumMap = coerce (foldMap :: (a -> Alt m b) -> f a -> Alt m b)

newtype PrettyShow a = PrettyShow a

instance (Show a) => Pretty (PrettyShow a) where
  pPrint (PrettyShow a) = P.text $ show a

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

newIORef :: (MonadIO m) => a -> m (IORef a)
newIORef = liftIO . IORef.newIORef

readIORef :: (MonadIO m) => IORef a -> m a
readIORef ref = liftIO $ IORef.readIORef ref

modifyIORef :: (MonadIO m) => IORef a -> (a -> a) -> m ()
modifyIORef ref f = liftIO $ IORef.modifyIORef ref f

writeIORef :: (MonadIO m) => IORef a -> a -> m ()
writeIORef ref a = liftIO $ IORef.writeIORef ref a

class Container c v | c -> v where
  singleton :: v -> c

instance Container [a] a where
  singleton = pure

instance (Hashable a) => Container (HashSet a) a where
  singleton = HashSet.singleton

instance (Hashable k) => Container (HashMap k v) (k, v) where
  singleton = uncurry HashMap.singleton

instance Container (Map k v) (k, v) where
  singleton = uncurry Map.singleton

{-# DEPRECATED one "use singleton" #-}
one :: (Container c v) => v -> c
one = singleton

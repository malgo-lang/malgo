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
    putText,

    -- ** IORef
    newIORef,
    readIORef,
    modifyIORef,
    writeIORef,
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
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Int (Int32, Int64)
import Data.Kind (Constraint)
import Data.List (dropWhileEnd, foldl', sort, transpose)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
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
import Prettyprinter
import System.IO (Handle, stderr, stdin, stdout)
import System.IO qualified
import Prelude hiding (id, unzip)

identity :: a -> a
identity x = x
{-# INLINE identity #-}

pass :: (Applicative f) => f ()
pass = pure ()
{-# INLINE pass #-}

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
{-# INLINE replaceOf #-}

chomp :: String -> String
chomp = dropWhileEnd (`elem` ['\r', '\n'])
{-# INLINE chomp #-}

asumMap :: forall b m f a. (Foldable f, Alternative m) => (a -> m b) -> f a -> m b
asumMap = coerce (foldMap :: (a -> Alt m b) -> f a -> Alt m b)
{-# INLINE asumMap #-}

newtype PrettyShow a = PrettyShow a

instance (Show a) => Pretty (PrettyShow a) where
  pretty (PrettyShow a) = pretty $ convertString @_ @Text $ show a

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

putText :: (MonadIO m) => Text -> m ()
putText = hPutText stdout

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

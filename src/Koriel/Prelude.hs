{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Prelude
  ( -- * Reexports
    module Relude,
    module Control.Monad.Writer.Class,
    module Control.Monad.Trans.Writer.CPS,

    -- * Utilities
    unzip,
    replaceOf,
    localState,
    chomp,

    -- * Lift IO functions

    -- ** Show
    hPrint,

    -- ** String
    hPutStr,
    hPutStrLn,

    -- ** Text
    hPutText,
    hPutTextLn,
  )
where

import Control.Lens (ASetter, over)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Trans.Writer.CPS qualified as W
import Control.Monad.Writer.Class hiding (pass)
import Control.Monad.Writer.Class qualified as Writer
import Data.List (dropWhileEnd)
import Data.Monoid
import Data.Text.IO qualified as T
import Relude hiding (All, Op, Type, id, unzip)
import System.IO qualified

-- | Generalization of 'Data.List.unzip' :: [(a, b)] -> ([a], [b])
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

chomp :: String -> String
chomp = dropWhileEnd (`elem` ['\r', '\n'])

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  tell = W.tell
  listen = W.listen
  pass = W.pass

instance MonadState s m => MonadState s (WriterT w m) where
  state = lift . state

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  reader = lift . reader
  local = W.mapWriterT . local

-- Lift IO funcitons

-- | Lifted version of 'System.IO.hPrint'.
hPrint :: (MonadIO m, Show a) => Handle -> a -> m ()
hPrint handle x = liftIO $ System.IO.hPrint handle x

-- | Lifted version of 'System.IO.hPutStr'.
hPutStr :: MonadIO m => Handle -> String -> m ()
hPutStr handle x = liftIO $ System.IO.hPutStr handle x

-- | Lifted version of 'System.IO.hPutStrLn'.
hPutStrLn :: MonadIO m => Handle -> String -> m ()
hPutStrLn handle x = liftIO $ System.IO.hPutStrLn handle x

-- | Lifted version of 'T.hPutStr'.
hPutText :: MonadIO m => Handle -> Text -> m ()
hPutText handle x = liftIO $ T.hPutStr handle x

-- | Lifted version of 'T.hPutStrLn'.
hPutTextLn :: MonadIO m => Handle -> Text -> m ()
hPutTextLn handle x = liftIO $ T.hPutStrLn handle x
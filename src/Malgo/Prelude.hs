module Malgo.Prelude (module Prelude, identity, hPutStr, hPutStrLn, putStr, putStrLn, print) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.String.Conversions (ConvertibleStrings, cs)
import System.IO (Handle, stdout)
import Prelude hiding (id, print, putStr, putStrLn)

identity :: a -> a
identity x = x
{-# INLINE identity #-}

-- | Lifted version of @Data.ByteString.hPut@.
-- Blocks until the whole string is written.
hPutStr :: (ConvertibleStrings str ByteString, MonadIO m) => Handle -> str -> m ()
hPutStr h = liftIO . BS.hPut h . cs

-- | Lifted version of @Data.ByteString.hPut@, with a newline appended.
-- Blocks until the whole string is written.
hPutStrLn :: (ConvertibleStrings str ByteString, MonadIO m) => Handle -> str -> m ()
hPutStrLn h str = liftIO $ BS.hPut h $ cs str <> "\n"

putStr :: (ConvertibleStrings str ByteString, MonadIO m) => str -> m ()
putStr = hPutStr stdout

putStrLn :: (ConvertibleStrings str ByteString, MonadIO m) => str -> m ()
putStrLn = hPutStrLn stdout

print :: (Show a, MonadIO m) => a -> m ()
print = putStr . show
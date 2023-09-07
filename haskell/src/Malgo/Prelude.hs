{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Prelude (module Prelude, identity, hPutStr, hPutStrLn, putStr, putStrLn, print) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Conversions (ConvertibleStrings, cs)
import Prettyprinter
import System.IO (Handle, stdout)
import Text.Parsec (SourcePos)
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

instance Pretty SourcePos where
  pretty = pretty . show

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = pretty . Map.toList
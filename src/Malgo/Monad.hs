-- | Utility module for ReaderT pattern
module Malgo.Monad (MalgoM, runMalgoM, newCtx, MonadMalgo (..)) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.IORef
import Malgo.Prelude

newtype Ctx = Ctx
  { uniqSupply :: IORef Int
  }

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT Ctx IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runMalgoM :: Ctx -> MalgoM a -> IO a
runMalgoM ctx m = runReaderT m.unMalgoM ctx

newCtx :: IO Ctx
newCtx = Ctx <$> newIORef 0

class (Monad m) => MonadMalgo m where
  newUniq :: m Int

instance MonadMalgo MalgoM where
  newUniq = MalgoM $ do
    ctx <- ask
    liftIO $ atomicModifyIORef' ctx.uniqSupply (\x -> (x + 1, x))
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}
module Language.Malgo.Monad
  ( UniqSupply(..)
  , MalgoApp(..)
  , runMalgo
  , MonadMalgo(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           RIO
import           RIO.Process
import           System.Environment   (lookupEnv)

newtype UniqSupply = UniqSupply { unUniqSupply :: IORef Int }

data MalgoApp = MalgoApp
  { maLogFunc        :: LogFunc
  , maProcessContext :: ProcessContext
  , maUniqSupply     :: UniqSupply
  }

instance HasLogFunc MalgoApp where
  logFuncL = lens maLogFunc (\x y -> x { maLogFunc = y })
instance HasProcessContext MalgoApp where
  processContextL = lens maProcessContext (\x y -> x { maProcessContext = y})

runMalgo :: MonadIO m => RIO MalgoApp a -> UniqSupply -> m a
runMalgo m u = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERVOSE"
  lo <- logOptionsHandle stderr verbose
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    runRIO (MalgoApp lf pc u) m

class Monad m => MonadMalgo m where
  newUniq :: m Int
  liftApp :: RIO MalgoApp a -> m a

instance MonadMalgo (RIO MalgoApp) where
  newUniq = do
    UniqSupply u <- maUniqSupply <$> ask
    i <- readIORef u
    modifyIORef u (+1)
    return i
  liftApp = id

instance MonadMalgo m => MonadMalgo (ReaderT r m) where
  newUniq = lift newUniq
  liftApp = lift . liftApp

instance MonadMalgo m => MonadMalgo (ExceptT e m) where
  newUniq = lift newUniq
  liftApp = lift . liftApp

instance MonadMalgo m => MonadMalgo (StateT s m) where
  newUniq = lift newUniq
  liftApp = lift . liftApp

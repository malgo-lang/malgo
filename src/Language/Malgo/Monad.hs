{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.Malgo.Monad
  ( UniqSupply(..)
  , MalgoApp(..)
  , runMalgo
  , newUniq
  ) where

import           Control.Monad.State
import           RIO
import           RIO.Process
import           System.Environment        (lookupEnv)

newtype UniqSupply = UniqSupply { unUniqSupply :: IORef Int }

data MalgoApp = MalgoApp
  { maLogFunc        :: !LogFunc
  , maProcessContext :: !ProcessContext
  , maUniqSupply     :: !UniqSupply
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
  withLogFunc lo $ \lf -> do
    let malgoApp = MalgoApp { maLogFunc = lf, maProcessContext = pc, maUniqSupply = u }
    runRIO malgoApp m

newUniq :: RIO MalgoApp Int
newUniq = do
  UniqSupply u <- maUniqSupply <$> ask
  i <- readIORef u
  modifyIORef u (+1)
  return i

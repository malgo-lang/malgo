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
  , newUniq
  , Opt(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           RIO
import           RIO.Process
import           System.Environment   (lookupEnv)

newtype UniqSupply = UniqSupply { unUniqSupply :: IORef Int }

data Opt = Opt
  { _srcName       :: Text
  , _dumpParsed    :: Bool
  , _dumpRenamed   :: Bool
  , _dumpTyped     :: Bool
  , _dumpKNormal   :: Bool
  , _dumpTypeTable :: Bool
  , _dumpClosure   :: Bool
  , _isDebugMode   :: Bool
  } deriving (Eq, Show)

data MalgoApp = MalgoApp
  { maLogFunc        :: LogFunc
  , maProcessContext :: ProcessContext
  , maUniqSupply     :: UniqSupply
  , maOption         :: Opt
  }

instance HasLogFunc MalgoApp where
  logFuncL = lens maLogFunc (\x y -> x { maLogFunc = y })

instance HasProcessContext MalgoApp where
  processContextL = lens maProcessContext (\x y -> x { maProcessContext = y})

runMalgo :: MonadIO m => RIO MalgoApp a -> UniqSupply -> Opt -> m a
runMalgo m u opt = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERVOSE"
  lo <- logOptionsHandle stderr verbose
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    runRIO (MalgoApp lf pc u opt) m

class Monad m => MonadMalgo m where
  liftApp :: RIO MalgoApp a -> m a

instance MonadMalgo (RIO MalgoApp) where
  liftApp = id
instance MonadMalgo m => MonadMalgo (ReaderT r m) where
  liftApp = lift . liftApp
instance MonadMalgo m => MonadMalgo (ExceptT e m) where
  liftApp = lift . liftApp
instance MonadMalgo m => MonadMalgo (StateT s m) where
  liftApp = lift . liftApp

newUniq :: MonadMalgo m => m Int
newUniq = liftApp $ do
  UniqSupply u <- maUniqSupply <$> ask
  i <- readIORef u
  modifyIORef u (+1)
  return i

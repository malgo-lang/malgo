{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.Malgo.Monad
  ( UniqSupply(..)
  , MalgoM(..)
  , MalgoEnv(..)
  , runMalgo
  , MonadMalgo(..)
  , newUniq
  , Opt(..)
  , Colog.HasLog
  , Colog.Message
  , logDebug
  , Colog.logInfo
  , Colog.logWarning
  , Colog.logError
  , Colog.logException
  )
where

import           Colog                   hiding ( logDebug )
import qualified Colog
import           Control.Monad.Fix
import           Control.Monad.Trans.Writer.CPS
import           Language.Malgo.Prelude

newtype UniqSupply = UniqSupply (IORef Int)

data Opt = Opt
  { srcName       :: String
  , dstName       :: String
  , dumpParsed    :: Bool
  , dumpRenamed   :: Bool
  , dumpTyped     :: Bool
  , dumpKNormal   :: Bool
  , dumpTypeTable :: Bool
  , dumpMutRec    :: Bool
  , dumpClosure   :: Bool
  , dumpLIR       :: Bool
  , isDebugMode   :: Bool
  } deriving (Eq, Show)

data MalgoEnv m = MalgoEnv
  { maUniqSupply :: UniqSupply
  , maOption     :: Opt
  , maLogAction :: LogAction m Message
  }

instance HasLog (MalgoEnv m) Message m where
  getLogAction = maLogAction
  setLogAction newLogAction env = env { maLogAction = newLogAction }

newtype MalgoM a = MalgoM { unMalgoM :: ReaderT (MalgoEnv MalgoM) IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadReader (MalgoEnv MalgoM), MonadIO, MonadFix, MonadFail)

runMalgo :: MonadIO m => MalgoM a -> UniqSupply -> Opt -> m a
runMalgo (MalgoM m) u opt =
  liftIO $ runReaderT m (MalgoEnv u opt richMessageAction)

logDebug :: MonadMalgo m => Text -> m ()
logDebug msg = liftMalgo $ do
  opt <- asks maOption
  when (isDebugMode opt) $ Colog.logDebug msg

class MonadIO m => MonadMalgo m where
  liftMalgo :: MalgoM a -> m a

instance MonadMalgo MalgoM where
  liftMalgo = id
instance MonadMalgo m => MonadMalgo (ReaderT r m) where
  liftMalgo = lift . liftMalgo
instance MonadMalgo m => MonadMalgo (ExceptT e m) where
  liftMalgo = lift . liftMalgo
instance MonadMalgo m => MonadMalgo (StateT s m) where
  liftMalgo = lift . liftMalgo
instance MonadMalgo m => MonadMalgo (WriterT w m) where
  liftMalgo = lift . liftMalgo

newUniq :: MonadMalgo m => m Int
newUniq = liftMalgo $ do
  UniqSupply u <- asks maUniqSupply
  i            <- readIORef u
  modifyIORef u (+ 1)
  return i

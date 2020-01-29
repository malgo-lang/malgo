{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.Malgo.Monad
  ( MalgoM(..)
  , MalgoEnv(..)
  , runMalgo
  , MonadMalgo(..)
  , Opt(..)
  , Colog.HasLog
  , Colog.Message
  , Colog.logDebug
  , Colog.logInfo
  , Colog.logWarning
  , Colog.logError
  , newUniq
  , getFileName
  )
where

import           Colog                          ( HasLog(..)
                                                , Message
                                                , LogAction(..)
                                                , richMessageAction
                                                , cfilter
                                                )
import qualified Colog
import           Control.Monad.Fix
import           Language.Malgo.Prelude

data Opt = Opt
  { srcName       :: String
  , dstName       :: String
  , dumpParsed    :: Bool
  , dumpRenamed   :: Bool
  , dumpTyped     :: Bool
  , dumpKNormal   :: Bool
  , dumpTypeTable :: Bool
  , dumpClosure   :: Bool
  , dumpLIR       :: Bool
  , isDebugMode   :: Bool
  } deriving (Eq, Show)

data MalgoEnv m = MalgoEnv
  { maOption     :: Opt
  , maLogAction  :: LogAction m Message
  }

newtype MalgoState = MalgoState { maUniqSupply :: Int }

instance HasLog (MalgoEnv m) Message m where
  getLogAction = maLogAction
  setLogAction newLogAction env = env { maLogAction = newLogAction }

newtype MalgoM a = MalgoM { unMalgoM :: ReaderT (MalgoEnv MalgoM) (StateT MalgoState IO) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadReader (MalgoEnv MalgoM), MonadState MalgoState, MonadIO, MonadFix, MonadFail)

runMalgo :: MonadIO m => MalgoM a -> Opt -> m a
runMalgo (MalgoM m) opt = liftIO $ evaluatingStateT (MalgoState 0) $ runReaderT
  m
  MalgoEnv
    { maOption    = opt
    , maLogAction = if isDebugMode opt
                      then richMessageAction
                      else cfilter (\(Colog.Msg sev _ _) -> sev > Colog.Debug) richMessageAction
    }

class Monad m => MonadMalgo m where
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
  i <- maUniqSupply <$> get
  modify (\s -> s { maUniqSupply = i + 1 })
  return i

getFileName :: (MonadMalgo m, IsString a) => m a
getFileName = liftMalgo $ asks (fromString . srcName . maOption)

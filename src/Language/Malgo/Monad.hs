{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
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
  , getFileName
  , MonadUniq(..)
  )
where

import           Language.Malgo.Prelude

import           Colog                          ( HasLog(..)
                                                , Message
                                                , LogAction(..)
                                                , richMessageAction
                                                , cfilter
                                                )
import qualified Colog
import           Control.Monad.Fix

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
  } deriving stock (Eq, Show)

data MalgoEnv m = MalgoEnv
  { maOption     :: Opt
  , maSource :: Text
  , maLogAction  :: LogAction m Message
  }

newtype UniqSupply = UniqSupply { uniqSupply :: Int }

instance HasLog (MalgoEnv m) Message m where
  getLogAction = maLogAction
  setLogAction newLogAction env = env { maLogAction = newLogAction }

newtype MalgoM a = MalgoM { unMalgoM :: ReaderT (MalgoEnv MalgoM) (StateT UniqSupply IO) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadReader (MalgoEnv MalgoM), MonadState UniqSupply, MonadIO, MonadFix, MonadFail)

runMalgo :: MonadIO m => MalgoM a -> Opt -> Text -> m a
runMalgo (MalgoM m) opt source = liftIO $ evaluatingStateT (UniqSupply 0) $ runReaderT
  m
  MalgoEnv
    { maOption    = opt
    , maSource    = source
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

getFileName :: (MonadMalgo m, IsString a) => m a
getFileName = liftMalgo $ asks (fromString . srcName . maOption)

class Monad m => MonadUniq m where
  getUniqSupply :: m UniqSupply
  getUniq :: m Int

instance MonadUniq MalgoM where
  getUniqSupply = get
  getUniq       = do
    i <- uniqSupply <$> getUniqSupply
    modify (\s -> s { uniqSupply = i + 1 })
    pure i
instance MonadUniq m => MonadUniq (ReaderT r m) where
  getUniqSupply = lift getUniqSupply
  getUniq       = lift getUniq
instance MonadUniq m => MonadUniq (ExceptT e m) where
  getUniqSupply = lift getUniqSupply
  getUniq       = lift getUniq
instance MonadUniq m => MonadUniq (StateT s m) where
  getUniqSupply = lift getUniqSupply
  getUniq       = lift getUniq
instance MonadUniq m => MonadUniq (WriterT w m) where
  getUniqSupply = lift getUniqSupply
  getUniq       = lift getUniq

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Malgo.Monad
  ( UniqSupply
  , newUniqSupply
  , Opt(..)
  , MalgoEnv(..)
  , MalgoM(..)
  , runMalgo
  , MonadMalgo(..)
  , newUniq
  )
where

import           Universum

newtype UniqSupply = UniqSupply (IORef Int)

newUniqSupply :: MonadIO m => m UniqSupply
{-# SPECIALIZE newUniqSupply :: IO UniqSupply #-}
newUniqSupply = UniqSupply <$> newIORef 0

data Opt = Opt

data MalgoEnv = MalgoEnv
  { uniqSupply :: UniqSupply
  , option     :: Opt
  }

newtype MalgoM a = MalgoM { unMalgoM :: ReaderT MalgoEnv IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadReader MalgoEnv, MonadIO, MonadFail)

runMalgo :: MonadIO m => MalgoM a -> UniqSupply -> Opt -> m a
{-# SPECIALIZE runMalgo :: MalgoM a -> UniqSupply -> Opt -> IO a #-}
runMalgo (MalgoM m) u opt = liftIO $ runReaderT m (MalgoEnv u opt)

class (MonadIO m, MonadFail m) => MonadMalgo m where
  liftMalgo :: MalgoM a -> m a

instance MonadMalgo MalgoM where
  liftMalgo = id
instance MonadMalgo m => MonadMalgo (ReaderT r m) where
  liftMalgo = lift . liftMalgo
instance MonadMalgo m => MonadMalgo (ExceptT e m) where
  liftMalgo = lift . liftMalgo
instance MonadMalgo m => MonadMalgo (StateT s m) where
  liftMalgo = lift . liftMalgo

newUniq :: MonadMalgo m => m Int
newUniq = liftMalgo $ do
  UniqSupply u <- asks uniqSupply
  i            <- readIORef u
  modifyIORef u (+ 1)
  return i

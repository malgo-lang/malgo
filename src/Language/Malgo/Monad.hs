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

import           Control.Applicative         (Alternative)
import qualified Control.Monad.Except        as E
import           Control.Monad.Fail          (MonadFail)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Control.Monad.Reader        as R
import qualified Control.Monad.State.Lazy    as SL
import qualified Control.Monad.State.Strict  as SS
import           Control.Monad.Trans.Class   (lift)
import qualified Control.Monad.Writer.Lazy   as WL
import qualified Control.Monad.Writer.Strict as WS
import qualified Data.IORef                  as I

newtype UniqSupply = UniqSupply (I.IORef Int)

newUniqSupply :: MonadIO m => m UniqSupply
newUniqSupply = UniqSupply <$> liftIO (I.newIORef 0)

data Opt = Opt

data MalgoEnv = MalgoEnv
  { uniqSupply :: UniqSupply
  , option     :: Opt
  }

newtype MalgoM a = MalgoM { unMalgoM :: R.ReaderT MalgoEnv IO a }
  deriving (Functor, Applicative, Alternative, Monad, R.MonadReader MalgoEnv, MonadIO, MonadFail)

runMalgo :: MonadIO m => UniqSupply -> Opt -> MalgoM a -> m a
runMalgo u opt (MalgoM m) = liftIO $ R.runReaderT m (MalgoEnv u opt)

class (MonadIO m, MonadFail m) => MonadMalgo m where
  liftMalgo :: MalgoM a -> m a

instance MonadMalgo MalgoM where
  liftMalgo = id
instance MonadMalgo m => MonadMalgo (R.ReaderT r m) where
  liftMalgo = lift . liftMalgo
instance MonadMalgo m => MonadMalgo (E.ExceptT e m) where
  liftMalgo = lift . liftMalgo
instance MonadMalgo m => MonadMalgo (SS.StateT s m) where
  liftMalgo = lift . liftMalgo
instance MonadMalgo m => MonadMalgo (SL.StateT s m) where
  liftMalgo = lift . liftMalgo
instance (MonadMalgo m, Monoid w) => MonadMalgo (WS.WriterT w m) where
  liftMalgo = lift . liftMalgo
instance (MonadMalgo m, Monoid w) => MonadMalgo (WL.WriterT w m) where
  liftMalgo = lift . liftMalgo

newUniq :: MonadMalgo m => m Int
newUniq = liftMalgo $ do
  UniqSupply u <- R.asks uniqSupply
  i            <- liftIO $ I.readIORef u
  liftIO $ I.modifyIORef u (+ 1)
  return i

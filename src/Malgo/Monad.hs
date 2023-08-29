-- | Utility module for ReaderT pattern
module Malgo.Monad
  ( MalgoM,
    runMalgoM,
    newCtx,
    MonadMalgo (..),
    Id (..),
    newId,
  )
where

import Control.Monad.Cont (ContT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.IORef
import Data.Text (Text)
import GHC.Generics (Generic)
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

instance (MonadMalgo m) => MonadMalgo (ReaderT r m) where
  newUniq = lift newUniq

instance (MonadMalgo m) => MonadMalgo (StateT s m) where
  newUniq = lift newUniq

instance (MonadMalgo m) => MonadMalgo (ContT r m) where
  newUniq = lift newUniq

instance (MonadMalgo m) => MonadMalgo (Lazy.StateT s m) where
  newUniq = lift newUniq

data Id = Id {name :: Text, uniq :: Int}
  deriving stock (Eq, Ord, Show, Generic)

newId :: (MonadMalgo m) => Text -> m Id
newId name = Id name <$> newUniq
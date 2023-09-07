-- | Utility module for ReaderT pattern
module Malgo.Monad
  ( MalgoM,
    runMalgoM,
    newCtx,
    MonadMalgo (..),
    Id (..),
    newId,
    shorten,
  )
where

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Writer.CPS (WriterT)
import Data.IORef
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Malgo.Prelude
import Prettyprinter (Pretty (pretty))

data Ctx = Ctx
  { uniqSupply :: IORef Int,
    sourceFilePath :: FilePath
  }

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT Ctx IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runMalgoM :: Ctx -> MalgoM a -> IO a
runMalgoM ctx m = runReaderT m.unMalgoM ctx

newCtx :: FilePath -> IO Ctx
newCtx sourceFilePath = do
  uniqSupply <- newIORef 0
  pure $ Ctx {..}

class (Monad m) => MonadMalgo m where
  newUniq :: m Int
  getSource :: m FilePath

instance MonadMalgo MalgoM where
  newUniq = MalgoM $ do
    ctx <- ask
    liftIO $ atomicModifyIORef' ctx.uniqSupply (\x -> (x + 1, x))
  getSource = MalgoM $ do
    ctx <- ask
    pure ctx.sourceFilePath

instance (MonadMalgo m) => MonadMalgo (ReaderT r m) where
  newUniq = lift newUniq
  getSource = lift getSource

instance (MonadMalgo m) => MonadMalgo (StateT s m) where
  newUniq = lift newUniq
  getSource = lift getSource

instance (MonadMalgo m) => MonadMalgo (ContT r m) where
  newUniq = lift newUniq
  getSource = lift getSource

instance (MonadMalgo m) => MonadMalgo (Lazy.StateT s m) where
  newUniq = lift newUniq
  getSource = lift getSource

instance (MonadMalgo m) => MonadMalgo (WriterT w m) where
  newUniq = lift newUniq
  getSource = lift getSource

instance (MonadMalgo m) => MonadMalgo (ExceptT e m) where
  newUniq = lift newUniq
  getSource = lift getSource

data Id = Id {scope :: FilePath, name :: Text, uniq :: Int}
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Id where
  pretty (Id scope name uniq) = pretty scope <> "/" <> pretty name <> "_" <> pretty uniq

newId :: (MonadMalgo m) => Text -> m Id
newId name = do
  scope <- getSource
  Id scope name <$> newUniq

shorten :: Id -> Text
shorten (Id _ name _) = cs [Text.head name]
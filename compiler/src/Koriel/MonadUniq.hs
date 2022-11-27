module Koriel.MonadUniq where

import Control.Lens (view)
import Koriel.Lens
import Koriel.Prelude
import Text.Show (Show (show))

newtype UniqSupply = UniqSupply {_uniqSupply :: IORef Int}
  deriving stock (Eq)

instance Show UniqSupply where
  show _ = "UniqSupply"

instance HasUniqSupply UniqSupply UniqSupply where
  uniqSupply = identity

getUniq :: (MonadIO m, HasUniqSupply env UniqSupply, MonadReader env m) => m Int
getUniq = do
  UniqSupply us <- view uniqSupply
  i <- readIORef us
  modifyIORef us (+ 1)
  pure i
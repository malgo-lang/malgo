module Koriel.MonadUniq (UniqSupply (..), HasUniqSupply, getUniq) where

import GHC.Records
import Koriel.Prelude

newtype UniqSupply = UniqSupply {uniqSupply :: IORef Int}
  deriving stock (Eq)

instance Show UniqSupply where
  show _ = "UniqSupply"

type HasUniqSupply r = HasField "uniqSupply" r UniqSupply

getUniq :: (MonadIO m, MonadReader r m, HasUniqSupply r) => m Int
getUniq = do
  UniqSupply us <- asks (.uniqSupply)
  i <- readIORef us
  modifyIORef us (+ 1)
  pure i

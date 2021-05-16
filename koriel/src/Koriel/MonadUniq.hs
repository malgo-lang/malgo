{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Koriel.MonadUniq where

import Koriel.Prelude

newtype UniqSupply = UniqSupply {_uniqSupply :: IORef Int}
  deriving stock (Eq)

instance Show UniqSupply where
  show _ = "UniqSupply"

class HasUniqSupply env where
  uniqSupply :: Lens' env UniqSupply

instance HasUniqSupply UniqSupply where
  uniqSupply = lens id const

getUniq :: (MonadIO m, HasUniqSupply env, MonadReader env m) => m Int
getUniq = do
  UniqSupply us <- view uniqSupply
  i <- readIORef us
  modifyIORef us (+ 1)
  pure i
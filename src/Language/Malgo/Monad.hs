{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.Malgo.Monad where

import           Control.Lens           (Lens', at, use, view, (.=))
import           Data.IORef
import qualified Data.Map               as Map
import           Language.Malgo.Prelude
import           Text.PrettyPrint       hiding ((<>))

newtype Malgo s a = Malgo { unMalgo :: StateT s IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadIO
           )

newtype UniqSupply = UniqSupply { unUniqSupply :: IORef Int }

class MalgoEnv s where
  uniqSupplyL :: Lens' s UniqSupply
  genEnv :: UniqSupply -> s

instance MalgoEnv UniqSupply where
  uniqSupplyL = identity
  genEnv = identity

class (MonadIO m, MalgoEnv s) => MonadMalgo s m | m -> s where
  newUniq :: m Int
  newUniq = do
    u <- getUniq
    setUniq (u + 1)
    return u

  setUniq :: Int -> m ()

  getUniq :: m Int

  addTable :: Ord k => [(k, v)] -> Lens' s (Map k v) -> m a -> m a
  lookupTable :: Ord k => Doc -> k -> Lens' s (Map k v) -> m v

instance MalgoEnv s => MonadMalgo s (Malgo s) where
  setUniq i' = do
    UniqSupply i <- use uniqSupplyL
    writeMutVar i i'

  getUniq = do
    UniqSupply i <- use uniqSupplyL
    readMutVar i

  addTable kvs l m = sandbox $ do
    s <- use l
    l .= (Map.fromList kvs <> s)
    m

  lookupTable err k l = do
    s <- use l
    case view (at k) s of
      Just x  -> pure x
      Nothing -> malgoError err

runMalgo :: MalgoEnv s => Malgo s a -> Int -> IO (a, s)
runMalgo (Malgo m) u = do
  i <- UniqSupply <$> newMutVar u
  runStateT m (genEnv i)

malgoError :: Doc -> Malgo s a2
malgoError mes = liftIO $ die $ show mes

newMutVar :: MonadIO m => a -> m (IORef a)
newMutVar x = liftIO $ newIORef x

readMutVar :: MonadIO m => IORef a -> m a
readMutVar r = liftIO $ readIORef r

writeMutVar :: MonadIO m => IORef a -> a -> m ()
writeMutVar r x = liftIO $ writeIORef r x

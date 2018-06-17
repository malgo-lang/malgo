{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.Malgo.Monad where

import           Control.Lens           (over, Lens', at, use, view, (.=))
import           Data.IORef
import qualified Data.Map               as Map
import           Language.Malgo.Prelude

newtype Malgo s a = Malgo { unMalgo :: StateT s IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadIO
           )

newtype Malgo' s a = Malgo' { unMalgo' :: ReaderT s IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader s
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

  getEnv :: m s

  addTable :: Ord k => [(k, v)] -> Lens' s (Map k v) -> m a -> m a
  lookupTable :: Ord k => Doc ann -> k -> Lens' s (Map k v) -> m v
  lookupTable err k l = do
    s <- view l <$> getEnv
    case view (at k) s of
      Just x  -> pure x
      Nothing -> malgoError err

instance MalgoEnv s => MonadMalgo s (Malgo s) where
  setUniq i' = do
    UniqSupply i <- use uniqSupplyL
    writeMutVar i i'

  getUniq = do
    UniqSupply i <- use uniqSupplyL
    readMutVar i

  getEnv = get

  addTable kvs l m = sandbox $
    use l >>= (l .=) . (Map.fromList kvs <>) >> m
      where
        sandbox action = do
          s <- get
          ret <- action
          put s
          return ret


instance MalgoEnv s => MonadMalgo s (Malgo' s) where
  setUniq i' = do
    UniqSupply i <- view uniqSupplyL
    writeMutVar i i'

  getUniq = do
    UniqSupply i <- view uniqSupplyL
    readMutVar i

  getEnv = ask

  addTable kvs l m =
    local (over l (Map.fromList kvs <>)) m

runMalgo :: MalgoEnv s => Malgo s a -> Int -> IO (a, s)
runMalgo (Malgo m) u = do
  i <- UniqSupply <$> newMutVar u
  runStateT m (genEnv i)

runMalgo' :: MalgoEnv s => Malgo' s a -> Int -> IO (a, s)
runMalgo' (Malgo' m) u = do
  i <- UniqSupply <$> newMutVar u
  runReaderT (do { a <- m
                 ; s <- ask
                 ; return (a, s)}) (genEnv i)

malgoError :: MonadMalgo s m => Doc ann -> m a
malgoError mes = liftIO $ die $ show mes

newMutVar :: MonadIO m => a -> m (IORef a)
newMutVar x = liftIO $ newIORef x

readMutVar :: MonadIO m => IORef a -> m a
readMutVar r = liftIO $ readIORef r

writeMutVar :: MonadIO m => IORef a -> a -> m ()
writeMutVar r x = liftIO $ writeIORef r x

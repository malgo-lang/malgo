{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
module Language.Malgo.Monad
  ( Malgo(..)
  , UniqSupply(..)
  , MalgoEnv(..)
  , MonadMalgo(..)
  , addTable
  , lookupTable
  , runMalgo
  , malgoError
  , IORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  ) where

import           Data.IORef             (IORef)
import qualified Data.IORef             as IORef
import qualified Data.Map               as Map
import           Language.Malgo.Prelude

newtype Malgo s a = Malgo { unMalgo :: ReaderT s IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader s
           , MonadIO
           )

newtype UniqSupply = UniqSupply { unUniqSupply :: IORef Int }

class MalgoEnv s where
  uniqSupplyL :: Lens' s UniqSupply
  genEnv :: UniqSupply -> IO s

instance MalgoEnv UniqSupply where
  uniqSupplyL = identity
  genEnv = return

class (MonadIO m, MalgoEnv s) => MonadMalgo s m | m -> s where
  newUniq :: m Int
  newUniq = do
    u <- getUniq
    setUniq (u + 1)
    return u

  setUniq :: Int -> m ()
  setUniq i = do
    UniqSupply u <- access uniqSupplyL
    writeIORef u i

  getUniq :: m Int
  getUniq = do
    UniqSupply u <- access uniqSupplyL
    readIORef u

  getEnv :: m s

  access :: Getter s a -> m a
  access l = do
    s <- getEnv
    return (view l s)

  change :: Setter' s v -> v -> m a -> m a

addTable :: (MonadMalgo s m, Ord k) => [(k, v)] -> Lens' s (Map k v) -> m a -> m a
addTable kvs l m = do
  tbl <- access l
  change l (Map.fromList kvs <> tbl) m

lookupTable :: (MonadMalgo s m, Ord k) => Doc ann -> k -> Lens' s (Map k v) -> m v
lookupTable err k l = do
  s <- access l
  case view (at k) s of
    Just x  -> pure x
    Nothing -> malgoError err

instance MalgoEnv s => MonadMalgo s (Malgo s) where
  getEnv = ask

  change l v m =
    local (over l (const v)) m

runMalgo :: MalgoEnv s => Malgo s a -> Int -> IO (a, s)
runMalgo (Malgo m) u = do
  i <- UniqSupply <$> newIORef u
  s <- genEnv i
  runReaderT ((,) <$> m <*> ask) s

malgoError :: MonadMalgo s m => Doc ann -> m a
malgoError mes = liftIO $ die $ show mes

newIORef :: MonadIO m => a -> m (IORef a)
newIORef x = liftIO $ IORef.newIORef x

readIORef :: MonadIO m => IORef a -> m a
readIORef r = liftIO $ IORef.readIORef r

writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef r x = liftIO $ IORef.writeIORef r x

modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef r f = liftIO $ IORef.modifyIORef r f

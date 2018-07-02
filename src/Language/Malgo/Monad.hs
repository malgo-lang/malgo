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
  , runMalgo'
  , malgoError
  , IORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  ) where

import           Data.Text.Prettyprint.Doc
import           RIO
import qualified RIO.Map                   as Map
import           RIO.Process
import           System.Environment        (lookupEnv)

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
  uniqSupplyL = id
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

  access :: SimpleGetter s a -> m a
  access l = do
    s <- getEnv
    return (view l s)

  change :: ASetter' s v -> v -> m a -> m a

addTable :: (MonadMalgo s m, Ord k) => [(k, v)] -> Lens' s (Map k v) -> m a -> m a
addTable kvs l m = do
  tbl <- access l
  change l (Map.fromList kvs <> tbl) m

lookupTable :: (MonadMalgo s m, Ord k) => Doc ann -> k -> Lens' s (Map k v) -> m v
lookupTable err k l = do
  s <- access l
  case Map.lookup k s of
    Just x  -> pure x
    Nothing -> malgoError err

instance MalgoEnv s => MonadMalgo s (Malgo s) where
  getEnv = ask

  change l v m =
    local (over l (const v)) m

instance MalgoEnv env => MonadMalgo env (RIO env) where
  getEnv = ask
  change l v m =
    local (over l (const v)) m

runMalgo :: (MonadIO m, MalgoEnv s) => Malgo s a -> Int -> m (a, s)
runMalgo (Malgo m) u = liftIO $ do
  i <- UniqSupply <$> newIORef u
  s <- genEnv i
  runReaderT ((,) <$> m <*> ask) s

runMalgo' :: (HasLogFunc env, HasProcessContext env, MalgoEnv env, MonadIO m) => RIO env a -> Int -> m a
runMalgo' m u = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERVOSE"
  lo <- logOptionsHandle stderr verbose
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf -> do
    env <- genEnv =<< UniqSupply <$> newIORef u
    runRIO (set processContextL pc (set logFuncL lf env)) m

malgoError :: MonadMalgo s m => Doc ann -> m a
malgoError mes = error $ show mes

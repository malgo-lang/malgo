{-# LANGUAGE FlexibleContexts           #-}
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
  , MalgoApp
  , runMalgo'
  , malgoError
  , newUniq'
  ) where

import           Control.Monad.State
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

data MalgoApp = MalgoApp
  { maLogFunc        :: !LogFunc
  , maProcessContext :: !ProcessContext
  , maUniqSupply     :: !UniqSupply
  }
instance HasLogFunc MalgoApp where
  logFuncL = lens maLogFunc (\x y -> x { maLogFunc = y })
instance HasProcessContext MalgoApp where
  processContextL = lens maProcessContext (\x y -> x { maProcessContext = y})

runMalgo' :: MonadIO m => RIO MalgoApp a -> UniqSupply -> m a
runMalgo' m u = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERVOSE"
  lo <- logOptionsHandle stderr verbose
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf -> do
    let malgoApp = MalgoApp { maLogFunc = lf, maProcessContext = pc, maUniqSupply = u }
    runRIO malgoApp m

malgoError :: MonadMalgo s m => Doc ann -> m a
malgoError mes = error $ show mes

newUniq' :: RIO MalgoApp Int
newUniq' = do
  UniqSupply u <- maUniqSupply <$> ask
  i <- readIORef u
  modifyIORef u (+1)
  return i

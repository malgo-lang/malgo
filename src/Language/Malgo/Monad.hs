{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
module Language.Malgo.Monad where

import           Control.Lens           (set, Setter', Lens', use, (+=), (.=))
import           Data.Generics.Product
import           Data.IORef
import qualified Data.Map as Map
import           Language.Malgo.Prelude
import           Text.PrettyPrint hiding ((<>))

newtype Malgo s a = Malgo { unMalgo :: StateT s IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadIO
           )

newtype UniqSupply = UniqSupply Int
  deriving (Show, Num, Default)

class HasUniqSupply s where
  uniqSupply :: Lens' s UniqSupply
  default uniqSupply :: (Generic s, HasField' "_uniqSupply" s UniqSupply) => Lens' s UniqSupply
  uniqSupply = field @"_uniqSupply"

instance HasUniqSupply UniqSupply where
  uniqSupply = identity

class (Monad m, HasUniqSupply s) => MonadMalgo s m | m -> s where
  newUniq :: m Int
  setUniq :: UniqSupply -> m ()
  getUniq :: m Int

  addTable :: Ord k => [(k, v)] -> Lens' s (Map k v) -> m a -> m a

instance HasUniqSupply s => MonadMalgo s (Malgo s) where
  newUniq = do
    UniqSupply i <- use uniqSupply
    uniqSupply += 1
    return i

  setUniq i = uniqSupply .= i

  getUniq = do
    UniqSupply i <- use uniqSupply
    return i

  addTable kvs l m = sandbox $ do
    s <- use l
    l .= (Map.fromList kvs <> s)
    m

runMalgo :: (Default s, HasUniqSupply s) => Malgo s a -> IO (a, s)
runMalgo (Malgo m) = runStateT m def

malgoError :: Doc -> Malgo s a2
malgoError mes = liftIO $ die $ show mes

newMutVar :: a -> Malgo s (IORef a)
newMutVar x = Malgo $ lift $ newIORef x

readMutVar :: IORef a -> Malgo s a
readMutVar r = Malgo $ lift $ readIORef r

writeMutVar :: IORef a -> a -> Malgo s ()
writeMutVar r x = Malgo $ lift $ writeIORef r x

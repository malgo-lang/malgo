{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
module Language.Malgo.Old.Monad where

import           Control.Lens           (Lens', use, (+=), (.=))
import           Data.Generics.Product
import           Data.IORef
import           Language.Malgo.Old.Prelude
import           Text.PrettyPrint

-- TODO: replace with `ReaderT (IORef s) IO a`
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

newUniq :: HasUniqSupply s => Malgo s Int
newUniq = do
  UniqSupply i <- use uniqSupply
  uniqSupply += 1
  return i

setUniq :: HasUniqSupply s => UniqSupply -> Malgo s ()
setUniq i = uniqSupply .= i

getUniq :: HasUniqSupply s => Malgo s Int
getUniq = do
  UniqSupply i <- use uniqSupply
  return i

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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Monad where

import Language.Malgo.Prelude
import Data.IORef
import Text.PrettyPrint

newtype Malgo s a = Malgo { unMalgo :: StateT s IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadIO
           )

class HasUniqSupply s where
  getUniqSupply :: s -> Int
  setUniqSupply :: Int -> s -> s

instance HasUniqSupply Int where
  getUniqSupply = identity
  setUniqSupply = const

newUniq :: HasUniqSupply s => Malgo s Int
newUniq = do
  s <- get
  let u = getUniqSupply s
  put $ setUniqSupply (u + 1) s
  return u

setUniq :: HasUniqSupply s => Int -> Malgo s ()
setUniq i = modify (setUniqSupply i)

getUniq :: HasUniqSupply s => Malgo s Int
getUniq = gets getUniqSupply

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

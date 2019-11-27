{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeFamilies           #-}
module Language.Malgo.Pass where

import           Control.Monad.Reader
import           Data.Outputable
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Relude

class (Outputable t, Pretty t) => Pass p s t | p -> s t where
  isDump :: Proxy p -> Opt -> Bool
  trans :: Proxy p -> s -> MalgoM t

dump :: (MonadReader MalgoEnv m, Outputable a, MonadIO m, Pretty a) => a -> m ()
dump x = do
  opt <- asks maOption
  if isDebugMode opt
  then print $ ppr x
  else print $ pPrint x

transform :: Pass p s t => Proxy p -> s -> MalgoM t
transform p s = do
  opt <- asks maOption
  t <- trans p s
  when (isDump p opt) $
    dump t
  return t

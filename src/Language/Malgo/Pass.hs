{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Malgo.Pass where

import Language.Malgo.Monad
import Data.Kind (Type)
import Data.Proxy
import Control.Monad.Reader
import Data.Outputable
import Language.Malgo.Pretty
import Relude

class (Outputable (Target p), Pretty (Target p)) => Pass p where
  type Source p :: Type
  type Target p :: Type
  isDump :: Proxy p -> Opt -> Bool
  trans :: Proxy p -> Source p -> MalgoM (Target p)
 
dump :: (MonadReader MalgoEnv m, Outputable a, MonadIO m, Pretty a) => a -> m ()
dump x = do
  opt <- asks maOption
  if isDebugMode opt
  then print $ ppr x
  else print $ pPrint x

transform :: Pass p => Proxy p -> Source p -> MalgoM (Target p)
transform p s = do
  opt <- asks maOption
  t <- trans p s
  when (isDump p opt) $
    dump t
  return t

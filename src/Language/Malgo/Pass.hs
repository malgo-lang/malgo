{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
module Language.Malgo.Pass where

import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Prelude

class Pass p s t | p -> s t where
  isDump :: Opt -> Bool
  trans :: s -> MalgoM t

dump :: (MonadReader MalgoEnv m, Show a, MonadIO m, Pretty a) => a -> m ()
dump x = do
  opt <- asks maOption
  if isDebugMode opt then putLTextLn $ pShow x else print $ pPrint x

transWithDump :: forall  p s t . (Pass p s t, Show t, Pretty t) => s -> MalgoM t
transWithDump s = do
  opt <- asks maOption
  t   <- trans @p s
  when (isDump @p opt) $ dump t
  return t

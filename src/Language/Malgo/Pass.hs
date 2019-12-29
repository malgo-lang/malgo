{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Pass where

import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Language.Malgo.Prelude

class Pass p s t | p -> s t where
  isDump :: Opt -> Bool
  trans :: s -> MalgoM t

dump :: (MonadMalgo m, Show a, MonadIO m, Pretty a) => a -> m ()
dump x = do
  opt <- liftMalgo $ asks maOption
  if isDebugMode opt
    then logDebug $ "dump:\n" <> toText (pShow x)
    else liftMalgo $ logInfo $ "dump:\n" <> show (pPrint x)

transWithDump :: forall  p s t . (Pass p s t, Show t, Pretty t) => s -> MalgoM t
transWithDump s = do
  opt <- asks maOption
  t   <- trans @p s
  when (isDump @p opt) $ dump t
  return t

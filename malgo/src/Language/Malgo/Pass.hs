{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Pass where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Koriel.Prelude
import Koriel.Pretty
import Language.Malgo.Monad

class Pass p s t | p -> s t where
  passName :: Text
  trans :: s -> MalgoM t

dump :: (MonadMalgo m, Show a, MonadIO m, Pretty a) => a -> m ()
dump x = do
  opt <- getOpt
  if isDebugMode opt
    then printLog $ TL.toStrict (pShow x)
    else printLog $ T.pack (renderStyle (style {lineLength = maxBound}) (pPrint x))

transWithDump :: forall p s t. (Pass p s t, Show t, Pretty t) => Bool -> s -> MalgoM t
transWithDump isDump s = do
  t <- trans @p s
  when isDump $ do
    printLog $ "dump " <> passName @p
    dump t
  pure t

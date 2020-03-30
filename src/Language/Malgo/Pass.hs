{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Malgo.Pass where

import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.PrettyPrint.HughesPJClass ( renderStyle
                                                , style
                                                , Style(..)
                                                )

class Pass p s t | p -> s t where
  passName :: Text
  isDump :: Opt -> Bool
  trans :: s -> MalgoM t

dump :: (MonadMalgo m, Show a, MonadIO m, Pretty a) => a -> m ()
dump x = do
  opt <- getOpt
  if isDebugMode opt
    then log Debug $ "\n" <> TL.toStrict (pShow x)
    else log Info $ "\n" <> T.pack (renderStyle (style { lineLength = maxBound }) (pPrint x))

transWithDump :: forall p s t . (Pass p s t, Show t, Pretty t) => s -> MalgoM t
transWithDump s = do
  opt <- asks maOption
  t   <- trans @p s
  when (isDump @p opt) $ do
    log Info $ "dump " <> passName @p
    dump t
  pure t

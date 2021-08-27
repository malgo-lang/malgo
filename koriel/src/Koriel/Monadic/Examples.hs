{-# LANGUAGE NoMonomorphismRestriction #-}

module Koriel.Monadic.Examples where

import Koriel.Id
import Koriel.MonadUniq
import Koriel.Monadic.Syntax
import Koriel.Prelude hiding (exp)

prelude :: (MonadIO m, HasUniqSupply env, MonadReader env m) => m Program
prelude = program do
  print_string_id <- newExternalId "print_string" (TFun [TString] (TNode [(0, [])])) (ModuleName "Prelude")
  defExt print_string_id "print_string"
  printStringId <- newExternalId "printString" (TFun [TString] (TNode [(0, [])])) (ModuleName "Prelude")
  preDef printStringId
  defVar printStringId =<< exp do
    arg0 <- newInternalId "arg0" TString
    var <-
      closure . Func [] [arg0] =<< exp do
        Unit . Var <$> apply Direct print_string_id [Var arg0]
    pure $ Unit $ Var var

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Malgo.RenameTest where

import Data.String.Conversions (ConvertibleStrings (..))
import Malgo.Monad (newCtx, runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Rename (rename)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

test_rename :: [TestTree]
test_rename =
  map
    aux
    [ ("identity", "{ # x -> x }"),
      ("const", "{ # x y -> x }"),
      ("shadowing", "{ # x -> { # x -> x } }")
    ]
  where
    aux (name, input) = goldenVsString ("rename " <> name) ("test/Malgo/Rename/golden/" <> name <> ".golden") do
      case parse "test.mlg" input of
        Left err -> pure $ convertString err
        Right result -> do
          ctx <- newCtx
          result' <- runMalgoM ctx (rename result)
          pure $ convertString $ show result'
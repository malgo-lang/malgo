{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Malgo.ParserTest where

import Data.String.Conversions (convertString)
import Malgo.Parser
import Malgo.Prelude
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

test_parser :: [TestTree]
test_parser =
  map
    aux
    [ ("variable", "x"),
      ("literal", "42"),
      ("apply", "f x y z"),
      ("this", "#"),
      ("comment", "# {- {- comment -} comment -}"),
      ("function", "{ # x -> x }")
    ]
  where
    aux (name, input) = goldenVsString ("parse " <> name) ("test/Malgo/Parser/golden/" <> name <> ".golden") do
      case parse "test.mlg" input of
        Left err -> pure $ convertString err
        Right result -> pure $ convertString $ show result

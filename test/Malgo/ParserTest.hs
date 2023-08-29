{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Malgo.ParserTest where

import Data.String.Conversions (convertString)
import Malgo.Parser
import Malgo.Prelude
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

test_var :: TestTree
test_var = goldenVsString "parse variable" "test/Malgo/Parser/golden/var.golden" do
  case parse "test.mlg" "x" of
    Left err -> pure $ convertString err
    Right result -> pure $ convertString $ show result

test_lit :: TestTree
test_lit = goldenVsString "parse literal" "test/Malgo/Parser/golden/lit.golden" do
  case parse "test.mlg" "42" of
    Left err -> pure $ convertString err
    Right result -> pure $ convertString $ show result

test_apply :: TestTree
test_apply = goldenVsString "parse apply" "test/Malgo/Parser/golden/apply.golden" do
  case parse "test.mlg" "f x y z" of
    Left err -> pure $ convertString err
    Right result -> pure $ convertString $ show result

test_this :: TestTree
test_this = goldenVsString "parse this" "test/Malgo/Parser/golden/this.golden" do
  case parse "test.mlg" "#" of
    Left err -> pure $ convertString err
    Right result -> pure $ convertString $ show result

test_comment :: TestTree
test_comment = goldenVsString "parse comment" "test/Malgo/Parser/golden/comment.golden" do
  case parse "test.mlg" "# {- {- comment -} comment -}" of
    Left err -> pure $ convertString err
    Right result -> pure $ convertString $ show result

test_function :: TestTree
test_function = goldenVsString "parse function" "test/Malgo/Parser/golden/function.golden" do
  case parse "test.mlg" "{ # x -> x }" of
    Left err -> pure $ convertString err
    Right result -> pure $ convertString $ show result
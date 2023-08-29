{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Malgo.ParserTest where

import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Conversions (convertString)
import Data.Text (Text)
import Malgo.Parser
import Malgo.Prelude
import Malgo.Syntax
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, (@?=))

unit_var :: Assertion
unit_var = parse "test.mlg" "x" @?= Right (Var "x")

unit_lit :: Assertion
unit_lit = do
  parse "test.mlg" "42" @?= Right (Lit $ Int 42)
  parse "test.mlg" "0" @?= Right (Lit $ Int 0)

unit_apply :: Assertion
unit_apply = do
  parse "test.mlg" "f x" @?= Right (App (Var "f") (Var "x" :| []))
  parse "test.mlg" "f x y" @?= Right (App (Var "f") (Var "x" :| [Var "y"]))
  parse "test.mlg" "f x y z" @?= Right (App (Var "f") (Var "x" :| [Var "y", Var "z"]))

unit_this :: Assertion
unit_this = do
  parse "test.mlg" "#" @?= Right (Var "#")

unit_comment :: Assertion
unit_comment = do
  parse "test.mlg" "# {- {- comment -} comment -}" @?= Right (Var "#")
  parse "test.mlg" "# -- comment" @?= Right (Var "#")

unit_function :: Assertion
unit_function = do
  parse "test.mlg" "{ # x -> x }" `mustAs` Codata (Clause (PApp PThis (PVar "x" :| [])) (Var "x") :| [])

mustAs :: (Eq a, Show a) => Either Text a -> a -> Assertion
mustAs (Right a) b = assertEqual "" a b
mustAs (Left err) b = do
  putStrLn err
  assertFailure $ "must be " <> show b

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
module Malgo.Parser.RegularSpec (spec) where

import Malgo.Monad (runMalgoM)
import Malgo.Parser.Regular (parseRegular)
import Malgo.Prelude
import Malgo.TestUtils
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec = describe "Regular Parser" do
  it "parses regular function application with spaces" do
    let src = "def main = f x y" :: String
    result <- runMalgoM flag do
      parseRegular "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "parses regular tuple syntax with parentheses" do
    let src = "def main = (x, y, z)" :: String
    result <- runMalgoM flag do
      parseRegular "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "parses regular function clauses without parentheses" do
    let src = "def f = { x y -> x }" :: String
    result <- runMalgoM flag do
      parseRegular "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "parses function applied to tuple (traditional syntax)" do
    let src = "def main = f(x, y)" :: String
    result <- runMalgoM flag do
      parseRegular "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "parses function with multiple clauses (traditional syntax)" do
    let src = "def main = {x, y}" :: String
    result <- runMalgoM flag do
      parseRegular "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

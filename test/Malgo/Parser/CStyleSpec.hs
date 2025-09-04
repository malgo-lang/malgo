module Malgo.Parser.CStyleSpec (spec) where

import Malgo.Monad (runMalgoM)
import Malgo.Parser.CStyle (parseCStyle)
import Malgo.Prelude
import Malgo.TestUtils
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec = describe "C-Style Parser" do
  it "parses C-style function calls with parentheses" do
    let src = "def main = f(x, y)" :: String
    result <- runMalgoM flag do
      parseCStyle "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "parses C-style tuple syntax with braces" do
    let src = "def main = {x, y}" :: String
    result <- runMalgoM flag do
      parseCStyle "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "parses C-style function clauses with parentheses" do
    let src = "def f = { (x, y) -> x }" :: String
    result <- runMalgoM flag do
      parseCStyle "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "parses empty function calls" do
    let src = "def main = f()" :: String
    result <- runMalgoM flag do
      parseCStyle "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "handles nested C-style calls" do
    let src = "def main = f(g(x), h(y, z))" :: String
    result <- runMalgoM flag do
      parseCStyle "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

module Malgo.Parser.WrapperSpec (spec) where

import Malgo.Monad (runMalgoM)
import Malgo.Parser.Wrapper (parseWithWrapper)
import Malgo.Prelude
import Malgo.TestUtils
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec = describe "Parser Wrapper" do
  it "routes to regular parser when no pragma detected" do
    let src = "def main = f x y" :: String
    result <- runMalgoM flag do
      parseWithWrapper "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "routes to C-style parser when #c-style-apply pragma present" do
    let src = "#c-style-apply\ndef main = f(x, y)" :: String
    result <- runMalgoM flag do
      parseWithWrapper "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "handles multiple pragmas correctly" do
    let src = "#experimental-feature\n#c-style-apply\ndef main = f(x, y)" :: String
    result <- runMalgoM flag do
      parseWithWrapper "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "preserves pragma extraction behavior" do
    let src = "#c-style-apply\n#debug\ndef main = f(x, y)" :: String
    result <- runMalgoM flag do
      parseWithWrapper "test.mlg" (convertString src)
    case result of
      Left err -> expectationFailure $ errorBundlePretty err
      Right _ -> pure ()

  it "fails gracefully with invalid syntax for selected parser" do
    let src = "#c-style-apply\ndef main = f x y" :: String -- Regular syntax with C-style pragma
    result <- runMalgoM flag do
      parseWithWrapper "test.mlg" (convertString src)
    case result of
      Left _ -> pure () -- Expected to fail when parser mismatch
      Right _ -> expectationFailure "Should fail with syntax mismatch"

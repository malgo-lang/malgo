module Malgo.Parser.CStyleSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Malgo.Monad (runMalgoM)
import Malgo.Parser.CStyle (parseCStyle)
import Malgo.Prelude
import Malgo.SExpr qualified as SExpr
import Malgo.TestUtils
import System.FilePath ((</>))
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

  -- Golden tests for C-style data definitions and type synonyms
  golden "CStyleDataDef" (driveCStyleParse (testcaseDir </> "CStyleDataDef.mlg"))
  golden "CStyleDataDef sexpr" (driveCStyleParseSExpr (testcaseDir </> "CStyleDataDef.mlg"))
  golden "CStyleTypeSynonym" (driveCStyleParse (testcaseDir </> "CStyleTypeSynonym.mlg"))
  golden "CStyleTypeSynonym sexpr" (driveCStyleParseSExpr (testcaseDir </> "CStyleTypeSynonym.mlg"))

-- Drive functions for C-style parsing golden tests
driveCStyleParse :: FilePath -> IO String
driveCStyleParse srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag do
    parsed <- parseCStyle srcPath src
    case parsed of
      Left err -> error $ errorBundlePretty err
      Right parsed ->
        pure $ pShowCompact parsed

driveCStyleParseSExpr :: FilePath -> IO String
driveCStyleParseSExpr srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag do
    parsed <- parseCStyle srcPath src
    case parsed of
      Left err -> error $ errorBundlePretty err
      Right parsed ->
        pure $ pShowCompact $ SExpr.toSExpr parsed

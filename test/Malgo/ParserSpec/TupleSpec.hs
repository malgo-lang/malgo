module Malgo.ParserSpec.TupleSpec (spec) where

import Data.List (isInfixOf)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.SExpr (ToSExpr (..))
import Malgo.TestUtils
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec = describe "Tuple Syntax" do
  describe "Brace tuple expressions" do
    it "parses simple tuple with braces" do
      parseExpr "{1, 2}" `shouldSatisfy` isTuple

    it "parses tuple with three elements" do
      parseExpr "{1, 2, 3}" `shouldSatisfy` isTuple

    it "parses empty tuple" do
      parseExpr "{}" `shouldSatisfy` isTuple

    it "rejects single element tuple" do
      parseExpr "{1}" `shouldSatisfy` isParseFailure

  describe "Brace tuple patterns" do
    it "parses simple tuple pattern with braces" do
      parsePattern "{x, y}" `shouldSatisfy` isTuplePattern

    it "parses tuple pattern with three elements" do
      parsePattern "{x, y, z}" `shouldSatisfy` isTuplePattern

    it "parses empty tuple pattern" do
      parsePattern "{}" `shouldSatisfy` isTuplePattern

    it "rejects single element tuple pattern" do
      parsePattern "{x}" `shouldSatisfy` isParseFailure

  describe "Disambiguation from records" do
    it "distinguishes tuple from record expression" do
      parseExpr "{x, y}" `shouldSatisfy` isTuple
      parseExpr "{x = y}" `shouldSatisfy` isRecord

    it "distinguishes tuple from record pattern" do
      parsePattern "{x, y}" `shouldSatisfy` isTuplePattern
      parsePattern "{x = y}" `shouldSatisfy` isRecordPattern

-- Helper functions for testing
parseExpr :: String -> Either String String
parseExpr input =
  let src = "def test = " <> convertString input
   in case unsafePerformIO $ runMalgoM flag do
        parsed <- parse "test.mlg" src
        case parsed of
          Left err -> pure $ Left $ errorBundlePretty err
          Right parsed -> pure $ Right $ sShow parsed of
        Left err -> Left err
        Right result -> Right result

parsePattern :: String -> Either String String
parsePattern input =
  let src = "def test = { " <> convertString input <> " -> 1 }"
   in case unsafePerformIO $ runMalgoM flag do
        parsed <- parse "test.mlg" src
        case parsed of
          Left err -> pure $ Left $ errorBundlePretty err
          Right parsed -> pure $ Right $ sShow parsed of
        Left err -> Left err
        Right result -> Right result

isTuple :: Either String String -> Bool
isTuple (Right result) = "Tuple" `isInfixOf` result
isTuple (Left _) = False

isTuplePattern :: Either String String -> Bool
isTuplePattern (Right result) = "TupleP" `isInfixOf` result
isTuplePattern (Left _) = False

isRecord :: Either String String -> Bool
isRecord (Right result) = "Record" `isInfixOf` result
isRecord (Left _) = False

isRecordPattern :: Either String String -> Bool
isRecordPattern (Right result) = "RecordP" `isInfixOf` result
isRecordPattern (Left _) = False

isParseFailure :: Either String String -> Bool
isParseFailure (Left _) = True
isParseFailure (Right _) = False

module Malgo.Parser.LayoutParserSpec (spec) where

import Data.ByteString qualified as BS
import Data.Traversable (for)
import Malgo.Parser.LayoutParser (parse)
import Malgo.Parser.Lexer (lex)
import Malgo.Prelude hiding (lex)
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec
  ( SpecWith,
    expectationFailure,
    it,
    runIO,
  )
import Text.Megaparsec (errorBundlePretty)

spec :: SpecWith ()
spec = do
  testCases <- runIO loadTestCases
  traverse_ test testCases

test :: (String, Text) -> SpecWith ()
test (name, input) = it name do
  case lex name input of
    Left err -> expectationFailure ("lex: " <> errorBundlePretty err)
    Right tokens -> case parse name tokens of
      Left err -> expectationFailure ("parse: " <> errorBundlePretty err)
      Right _ -> pass

testCaseDir :: FilePath
testCaseDir = "./test/testcases/malgo"

loadTestCases :: IO [(String, Text)]
loadTestCases = do
  files <- listDirectory testCaseDir <&> filter (".mlg" `isExtensionOf`)
  for files \file -> do
    let path = testCaseDir </> file
    input <- convertString <$> BS.readFile path
    pure (takeBaseName file, input)
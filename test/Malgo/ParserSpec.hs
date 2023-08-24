module Malgo.ParserSpec (spec) where

import Data.ByteString qualified as BS
import Data.Text.Lazy qualified as TL
import Data.Traversable (for)
import Koriel.Pretty
import Malgo.Parser (parseMalgo)
import Malgo.Prelude hiding (lex)
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec
  ( SpecWith,
    expectationFailure,
    it,
    parallel,
    runIO,
  )
import Text.Megaparsec (errorBundlePretty)

spec :: SpecWith ()
spec = do
  testCases <- runIO loadTestCases
  parallel $ traverse_ test testCases

test :: (String, TL.Text) -> SpecWith ()
test (name, input) = it name do
  case parseMalgo name input of
    Left err -> expectationFailure ("parse: " <> errorBundlePretty err)
    Right ast ->
      void $ writeFile ("./test/tmp" </> name <> ".old.ast") (show $ pretty ast)

testCaseDir :: FilePath
testCaseDir = "./test/testcases/malgo"

loadTestCases :: IO [(String, TL.Text)]
loadTestCases = do
  files <- listDirectory testCaseDir <&> filter (".mlg" `isExtensionOf`)
  for files \file -> do
    let path = testCaseDir </> file
    input <- convertString <$> BS.readFile path
    pure (takeBaseName file, input)
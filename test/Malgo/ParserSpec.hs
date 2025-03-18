module Malgo.ParserSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.TestUtils
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec = parallel do
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  golden "Builtin" (driveParse builtinPath)
  golden "Prelude" (driveParse preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveParse (testcaseDir </> testcase))

driveParse :: FilePath -> IO String
driveParse srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag option do
    parsed <- parseMalgo srcPath src
    case parsed of
      Left err -> pure $ errorBundlePretty err
      Right parsed ->
        pure $ pShowCompact parsed
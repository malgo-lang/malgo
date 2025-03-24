module Malgo.NewParserSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Malgo.Monad (runMalgoM)
import Malgo.NewParser (parse)
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
    parsed <- parse srcPath src
    case parsed of
      Left err -> error $ errorBundlePretty err
      Right parsed ->
        pure $ pShowCompact parsed
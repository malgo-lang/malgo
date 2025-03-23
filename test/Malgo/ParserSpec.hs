module Malgo.ParserSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Effectful.State.Static.Local (get)
import Malgo.Module (Pragma)
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
    golden (takeBaseName testcase <> " pragma") (drivePragma (testcaseDir </> testcase))

driveParse :: FilePath -> IO String
driveParse srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag option do
    parsed <- parseMalgo srcPath src
    case parsed of
      Left err -> error $ errorBundlePretty err
      Right parsed ->
        pure $ pShowCompact parsed

drivePragma :: FilePath -> IO String
drivePragma srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag option do
    _ <- parseMalgo srcPath src
    pragmas <- get @Pragma
    pure $ pShowCompact pragmas

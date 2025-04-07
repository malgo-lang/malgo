module Malgo.ParserSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.SExpr (ToSExpr (..))
import Malgo.TestUtils
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec = parallel do
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  golden "Builtin" (driveParse builtinPath)
  golden "Builtin sexpr" (driveParseSExpr builtinPath)
  golden "Prelude" (driveParse preludePath)
  golden "Prelude sexpr" (driveParseSExpr preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveParse (testcaseDir </> testcase))
    golden (takeBaseName testcase <> " sexpr") (driveParseSExpr (testcaseDir </> testcase))

driveParse :: FilePath -> IO String
driveParse srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag do
    parsed <- parse srcPath src
    case parsed of
      Left err -> error $ errorBundlePretty err
      Right (_, parsed) ->
        pure $ pShowCompact parsed

driveParseSExpr :: FilePath -> IO String
driveParseSExpr srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM flag do
    parsed <- parse srcPath src
    case parsed of
      Left err -> error $ errorBundlePretty err
      Right (_, parsed) ->
        pure $ sShow parsed
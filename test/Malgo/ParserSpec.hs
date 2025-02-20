module Malgo.ParserSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions.Monomorphic (toString)
import Error.Diagnose (TabSize (..), WithUnicode (..), prettyDiagnostic)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Error.Diagnose.Diagnostic (addFile)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.TestUtils
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec

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
      Left err -> do
        let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
        let diag' = addFile diag srcPath (toString src)
        let message =
              convertString
                $ PP.renderStrict
                $ PP.layoutPretty PP.defaultLayoutOptions
                $ PP.unAnnotate
                $ prettyDiagnostic WithUnicode (TabSize 4) diag'
        pure message
      Right parsed ->
        pure $ pShowCompact parsed
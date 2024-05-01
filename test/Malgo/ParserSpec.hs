module Malgo.ParserSpec (spec) where

import Data.ByteString.Lazy qualified as BL
import Data.String.Conversions.Monomorphic (toString)
import Error.Diagnose (TabSize (..), WithUnicode (..), prettyDiagnostic)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Error.Diagnose.Diagnostic (addFile)
import Malgo.Core.Optimize (OptimizeOption, defaultOptimizeOption)
import Malgo.Monad (CompileMode (LLVM), runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import Test.Hspec
import Test.Hspec.Golden
import Text.Pretty.Simple

testcaseDir :: FilePath
testcaseDir = "./test/testcases/malgo"

spec :: Spec
spec = parallel do
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    golden ("parse " <> takeBaseName testcase) (driveParse (testcaseDir </> testcase))

driveParse :: FilePath -> IO String
driveParse srcPath = do
  src <- convertString <$> BL.readFile srcPath
  runMalgoM LLVM flag option do
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
        pure $ convertString $ pShowNoColor parsed

flag :: Flag
flag = Flag {noOptimize = False, lambdaLift = False, debugMode = False, testMode = True}

option :: OptimizeOption
option = defaultOptimizeOption
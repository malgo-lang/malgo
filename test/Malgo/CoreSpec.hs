module Malgo.CoreSpec (spec) where

import Data.ByteString qualified as BS
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Malgo.Core.Parser qualified as Core
import Malgo.Desugar.Pass (desugar)
import Malgo.Infer.Pass (infer)
import Malgo.Monad (CompileMode (..), runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.TestUtils
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    golden ("parse core " <> takeBaseName testcase) (driveCore (testcaseDir </> testcase))

driveCore :: FilePath -> IO String
driveCore srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM LLVM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    (_, core) <- desugar tcEnv refined
    let rendered = render $ pretty core
    case Core.parse "<test>" rendered of
      Left err -> do
        let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
        let diag' = addFile diag "<test>" $ convertString rendered
        let message =
              convertString $
                PP.renderStrict $
                  PP.layoutPretty PP.defaultLayoutOptions $
                    PP.unAnnotate $
                      prettyDiagnostic WithUnicode (TabSize 4) diag'
        error message
      Right parsedCore -> pure $ pShowCompact parsedCore
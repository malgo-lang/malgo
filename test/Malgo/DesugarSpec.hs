module Malgo.DesugarSpec (spec) where

import Data.ByteString qualified as BS
import Error.Diagnose
import Malgo.Desugar.Pass (desugar)
import Malgo.Infer.Pass (infer)
import Malgo.Monad (CompileMode (..), runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.TestUtils
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
  golden "desugar Builtin" (driveDesugar builtinPath)
  golden "desugar Prelude" (driveDesugar preludePath)
  for_ testcases \testcase -> do
    golden ("desugar " <> takeBaseName testcase) (driveDesugar (testcaseDir </> testcase))

driveDesugar :: FilePath -> IO String
driveDesugar srcPath = do
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
    pure $ pShowCompact core

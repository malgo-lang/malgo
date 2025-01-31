module Malgo.RefineSpec (spec) where

import Data.ByteString qualified as BS
import Error.Diagnose
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
  golden "refine Builtin" (driveRefine builtinPath)
  golden "refine Prelude" (driveRefine preludePath)
  for_ testcases \testcase -> do
    golden ("refine " <> takeBaseName testcase) (driveRefine (testcaseDir </> testcase))

driveRefine :: FilePath -> IO String
driveRefine srcPath = do
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
    pure $ pShowCompact refined

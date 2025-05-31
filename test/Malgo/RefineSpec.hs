module Malgo.RefineSpec (spec) where

import Data.ByteString qualified as BS
import Malgo.Infer.Pass (infer)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  golden "Builtin" (driveRefine builtinPath)
  golden "Prelude" (driveRefine preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveRefine (testcaseDir </> testcase))

driveRefine :: FilePath -> IO String
driveRefine srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv, _) <- failIfError <$> infer rnEnv renamed
    refined <- refine tcEnv typed
    pure $ pShowCompact refined

module Malgo.NewRenameSpec (spec) where

import Data.ByteString qualified as BS
import Malgo.Driver (failIfError)
import Malgo.Monad (runMalgoM)
import Malgo.NewParser (parse)
import Malgo.Prelude
import Malgo.NewRename.Pass (rename)
import Malgo.NewRename.RnEnv qualified as RnEnv
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
  golden "Builtin" (driveRename builtinPath)
  golden "Prelude" (driveRename preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveRename (testcaseDir </> testcase))

driveRename :: FilePath -> IO String
driveRename srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    pure $ pShowCompact renamed
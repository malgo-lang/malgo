module Malgo.InferSpec (spec) where

import Data.ByteString qualified as BS
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Monad (runMalgoM)
import Malgo.NewRename.Pass (rename)
import Malgo.NewRename.RnEnv qualified as RnEnv
import Malgo.Parser (parse)
import Malgo.Prelude
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
  golden "Builtin" (driveInfer builtinPath)
  golden "Prelude" (driveInfer preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveInfer (testcaseDir </> testcase))

driveInfer :: FilePath -> IO String
driveInfer srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typedAst, _) <- infer rnEnv renamed
    pure $ pShowCompact typedAst
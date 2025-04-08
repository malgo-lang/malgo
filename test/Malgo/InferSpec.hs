module Malgo.InferSpec (spec) where

import Data.ByteString qualified as BS
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

errorcaseDir :: FilePath
errorcaseDir = "test/Malgo/InferSpec/errors"

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
  errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory errorcaseDir
  for_ errorcases \errorcase -> do
    golden ("error " <> takeBaseName errorcase) (driveErrorInfer (errorcaseDir </> errorcase))

driveInfer :: FilePath -> IO String
driveInfer srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typedAst, _, _) <- failIfError <$> infer rnEnv renamed
    pure $ pShowCompact typedAst

driveErrorInfer :: FilePath -> IO String
driveErrorInfer srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    result <- infer rnEnv renamed
    case result of
      Left (_, err) -> pure $ show err
      Right _ -> error $ "Expected error, but successfully inferred"
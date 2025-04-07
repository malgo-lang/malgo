module Malgo.RenameSpec (spec) where

import Data.ByteString qualified as BS
import Malgo.Driver (failIfError)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.SExpr (sShow)
import Malgo.TestUtils
import System.Directory
import System.FilePath
import Test.Hspec

errorcaseDir :: FilePath
errorcaseDir = "test/Malgo/RenameSpec/errors"

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  golden "Builtin" (driveRename builtinPath)
  golden "Builtin sexpr" (driveRenameSExpr builtinPath)
  golden "Prelude" (driveRename preludePath)
  golden "Prelude sexpr" (driveRenameSExpr preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveRename (testcaseDir </> testcase))
    golden (takeBaseName testcase <> " sexpr") (driveRenameSExpr (testcaseDir </> testcase))
  errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory errorcaseDir
  for_ errorcases \errorcase -> do
    golden ("error " <> takeBaseName errorcase) (driveErrorRename (errorcaseDir </> errorcase))

driveRename :: FilePath -> IO String
driveRename srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    pure $ pShowCompact renamed

driveErrorRename :: FilePath -> IO String
driveErrorRename srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    result <- rename rnEnv parsed
    case result of
      Left (_, err) -> pure $ show err
      Right _ -> error $ "Expected error, but successfully renamed"

driveRenameSExpr :: FilePath -> IO String
driveRenameSExpr srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    pure $ sShow renamed
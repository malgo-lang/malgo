module Malgo.RenameSpec (spec) where

import Data.ByteString qualified as BS
import Malgo.Driver (failIfError)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.SExpr (sShow)
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
  golden "Builtin sexpr" (driveRenameSExpr builtinPath)
  golden "Prelude" (driveRename preludePath)
  golden "Prelude sexpr" (driveRenameSExpr preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveRename (testcaseDir </> testcase))
    golden (takeBaseName testcase <> " sexpr") (driveRenameSExpr (testcaseDir </> testcase))

driveRename :: FilePath -> IO String
driveRename srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    pure $ pShowCompact renamed

driveRenameSExpr :: FilePath -> IO String
driveRenameSExpr srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    pure $ sShow renamed
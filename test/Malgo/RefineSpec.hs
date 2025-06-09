module Malgo.RefineSpec (spec) where

import Data.ByteString qualified as BS
import Malgo.Infer
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parse)
import Malgo.Pass
import Malgo.Prelude
import Malgo.Refine
import Malgo.Rename
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
  runMalgoM flag $ runCompileError do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- genBuiltinRnEnv
    (renamed, _) <- runPass RenamePass (parsed, rnEnv)
    (typed, tcEnv, _) <- runPass InferPass (renamed, rnEnv)
    refined <- runPass RefinePass (typed, tcEnv)
    pure $ pShowCompact refined

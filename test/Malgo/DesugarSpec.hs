module Malgo.DesugarSpec (spec) where

import Data.ByteString qualified as BS
import Malgo.Desugar.Pass (desugar)
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Monad (runMalgoM)
import Malgo.NewParser (parse)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.NewRename.Pass (rename)
import Malgo.NewRename.RnEnv qualified as RnEnv
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
  golden "Builtin" (driveDesugar builtinPath)
  golden "Prelude" (driveDesugar preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveDesugar (testcaseDir </> testcase))

driveDesugar :: FilePath -> IO String
driveDesugar srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    (_, core) <- desugar tcEnv refined
    pure $ sShow core

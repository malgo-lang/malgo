module Malgo.LinkSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Malgo.Core.Flat qualified as Flag
import Malgo.Desugar.Pass (desugar)
import Malgo.Driver (failIfError)
import Malgo.Infer.Pass (infer)
import Malgo.Interface (buildInterface)
import Malgo.Link qualified as Link
import Malgo.Monad (runMalgoM)
import Malgo.NewParser (parse)
import Malgo.NewRename.Pass (rename)
import Malgo.NewRename.RnEnv qualified as RnEnv
import Malgo.NewRename.RnState (RnState (..))
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.SExpr (sShow)
import Malgo.Syntax
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
  golden "Builtin" (driveLink builtinPath)
  golden "Prelude" (driveLink preludePath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveLink (testcaseDir </> testcase))

driveLink :: FilePath -> IO String
driveLink srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parse srcPath src >>= \case
        Left err -> error $ show err
        Right (_, parsed) -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, rnState) <- failIfError <$> rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    (dsState, core) <- desugar tcEnv refined
    core' <- runReader refined.moduleName $ Flag.normalize core
    let inf = buildInterface refined.moduleName rnState tcEnv dsState
    core'' <- Link.link inf core'
    pure $ sShow core''
module Malgo.Sequent.ToCoreSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Malgo.Infer.Pass (infer)
import Malgo.Monad (runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.SExpr (sShow)
import Malgo.Sequent.Core.Flat (flatProgram)
import Malgo.Sequent.ToCore (toCore)
import Malgo.Sequent.ToFun (toFun)
import Malgo.Syntax (Module (..))
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
  golden "Builtin" (driveToCore builtinPath)
  golden "Builtin zero" (driveZero builtinPath)
  golden "Prelude" (driveToCore preludePath)
  golden "Prelude zero" (driveZero builtinPath)
  for_ testcases \testcase -> do
    golden (takeBaseName testcase) (driveToCore (testcaseDir </> testcase))
    golden (takeBaseName testcase <> " zero") (driveZero (testcaseDir </> testcase))

driveToCore :: FilePath -> IO String
driveToCore srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore
    pure $ sShow program

driveZero :: FilePath -> IO String
driveZero srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    program <- runReader refined.moduleName $ toFun refined.moduleDefinition >>= toCore >>= flatProgram
    pure $ sShow program
module Malgo.LinkSpec (spec) where

import Data.ByteString qualified as BS
import Effectful.Reader.Static (runReader)
import Error.Diagnose
import Malgo.Core.Flat qualified as Flag
import Malgo.Core.Optimize (OptimizeOption (..), defaultOptimizeOption)
import Malgo.Desugar.Pass (desugar)
import Malgo.Driver qualified as Driver
import Malgo.Infer.Pass (infer)
import Malgo.Interface (buildInterface)
import Malgo.Link qualified as Link
import Malgo.Monad (CompileMode (..), runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
import Malgo.Syntax
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Hspec.Golden
import Text.Pretty.Simple (pShowNoColor)

spec :: Spec
spec = parallel do
  runIO do
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    golden ("link" <> takeBaseName testcase) (driveLink (testcaseDir </> testcase))

driveLink :: FilePath -> IO String
driveLink srcPath = do
  src <- convertString <$> BS.readFile srcPath
  runMalgoM LLVM flag option do
    parsed <-
      parseMalgo srcPath src >>= \case
        Left err -> error $ show err
        Right parsed -> pure parsed
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, rnState) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    (dsState, core) <- desugar tcEnv refined
    core' <- runReader refined.moduleName $ Flag.normalize core
    let inf = buildInterface refined.moduleName rnState tcEnv dsState
    core'' <- Link.link inf core'
    pure $ convertString $ pShowNoColor core''

testcaseDir :: FilePath
testcaseDir = "./test/testcases/malgo"

setupBuiltin :: IO ()
setupBuiltin =
  runMalgoM LLVM flag option do
    Driver.compile "./runtime/malgo/Builtin.mlg"

setupPrelude :: IO ()
setupPrelude =
  runMalgoM LLVM flag option do
    Driver.compile "./runtime/malgo/Prelude.mlg"

flag :: Flag
flag = Flag {noOptimize = False, lambdaLift = False, debugMode = False, testMode = True}

option :: OptimizeOption
option = defaultOptimizeOption
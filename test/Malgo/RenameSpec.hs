module Malgo.RenameSpec (spec) where

import Data.ByteString qualified as BS
import Error.Diagnose
import Malgo.Core.Optimize (OptimizeOption (..), defaultOptimizeOption)
import Malgo.Driver qualified as Driver
import Malgo.Monad (CompileMode (..), runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Rename.Pass (rename)
import Malgo.Rename.RnEnv qualified as RnEnv
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
    golden ("rename " <> takeBaseName testcase) (driveRename (testcaseDir </> testcase))

driveRename :: FilePath -> IO String
driveRename srcPath = do
  src <- convertString <$> BS.readFile srcPath
  let parsed = case parseMalgo srcPath src of
        Left err -> error $ show err
        Right parsed -> parsed
  runMalgoM srcPath LLVM flag option do
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, rnState) <- rename rnEnv parsed
    pure $ convertString $ pShowNoColor renamed <> "\n" <> pShowNoColor rnState

testcaseDir :: FilePath
testcaseDir = "./test/testcases/malgo"

setupBuiltin :: IO ()
setupBuiltin =
  runMalgoM "./runtime/malgo/Builtin.mlg" LLVM flag option do
    Driver.compile "./runtime/malgo/Builtin.mlg"

setupPrelude :: IO ()
setupPrelude =
  runMalgoM "./runtime/malgo/Prelude.mlg" LLVM flag option do
    Driver.compile "./runtime/malgo/Prelude.mlg"

flag :: Flag
flag = Flag {noOptimize = False, lambdaLift = False, debugMode = False, testMode = True}

option :: OptimizeOption
option = defaultOptimizeOption
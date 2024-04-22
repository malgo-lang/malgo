module Malgo.RefineSpec (spec) where

import Data.ByteString qualified as BS
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local
import Error.Diagnose
import Koriel.Core.Optimize (OptimizeOption (..), defaultOptimizeOption)
import Koriel.Id (ModuleName)
import Koriel.MonadUniq (Uniq)
import Malgo.Driver qualified as Driver
import Malgo.Infer.Pass (infer)
import Malgo.Interface (Interface, ModulePathList)
import Malgo.Lsp.Index (Index)
import Malgo.Monad (CompileMode (..), DstPath, runMalgoM)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude
import Malgo.Refine.Pass (refine)
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
    setupTestDir
    setupBuiltin
    setupPrelude
  testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
  for_ testcases \testcase -> do
    golden ("refine " <> takeBaseName testcase) (driveRefine (testcaseDir </> testcase))

driveRefine :: FilePath -> IO String
driveRefine srcPath = do
  src <- convertString <$> BS.readFile srcPath
  let parsed = case parseMalgo srcPath src of
        Left err -> error $ show err
        Right parsed -> parsed
  runMalgoEff srcPath do
    rnEnv <- RnEnv.genBuiltinRnEnv
    (renamed, _) <- rename rnEnv parsed
    (typed, tcEnv) <- infer rnEnv renamed
    refined <- refine tcEnv typed
    pure $ convertString $ pShowNoColor refined

testcaseDir :: FilePath
testcaseDir = "./test/testcases/malgo"

outputDir :: FilePath
outputDir = "./test/tmp/rename/malgo_test"

setupTestDir :: IO ()
setupTestDir = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True (outputDir </> "libs")

setupBuiltin :: IO ()
setupBuiltin =
  runMalgoEff "./runtime/malgo/Builtin.mlg" do
    Driver.compile "./runtime/malgo/Builtin.mlg"

setupPrelude :: IO ()
setupPrelude =
  runMalgoEff "./runtime/malgo/Prelude.mlg" $ do
    Driver.compile "./runtime/malgo/Prelude.mlg"

runMalgoEff ::
  FilePath ->
  Eff
    '[ Reader OptimizeOption,
       Reader ModulePathList,
       Reader Flag,
       Reader CompileMode,
       Reader DstPath,
       State Uniq,
       State (HashMap ModuleName Index),
       State (HashMap ModuleName Interface),
       IOE
     ]
    a ->
  IO a
runMalgoEff src action = do
  let dst = outputDir </> takeBaseName src -<.> "ll"
  let modPaths = [outputDir]
  runEff $ runMalgoM dst modPaths LLVM Flag {noOptimize = False, lambdaLift = False, debugMode = False, testMode = True} defaultOptimizeOption action
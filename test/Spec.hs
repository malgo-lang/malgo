{-# LANGUAGE CPP #-}
-- For `undefined`
{-# OPTIONS_GHC -Wno-deprecations #-}

import Data.String.Conversions (convertString)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Error.Diagnose (addFile, defaultStyle, printDiagnostic)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Extra (timeout)
import Koriel.Core.Annotate qualified as Koriel
import Koriel.Core.Lint qualified as Koriel
import Koriel.Core.Optimize (OptimizeOption (..), defaultOptimizeOption)
import Koriel.Core.Parser qualified as Koriel
import Koriel.Id (ModuleName (ModuleName))
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
import System.Directory (copyFile, listDirectory)
import System.Directory.Extra (createDirectoryIfMissing)
import System.FilePath (isExtensionOf, takeBaseName, takeDirectory, (-<.>), (</>))
import System.IO.Silently (hSilence)
import System.Process.Typed (
  ExitCode (ExitFailure, ExitSuccess),
  byteStringInput,
  nullStream,
  proc,
  readProcessStdout_,
  runProcess,
  setStderr,
  setStdin,
  setStdout, readProcessStdout, readProcessStderr,
 )
import Test.Hspec (
  anyException,
  describe,
  example,
  hspec,
  it,
  parallel,
  runIO,
  shouldThrow,
 )

testcaseDir :: FilePath
testcaseDir = "./test/testcases/malgo"

outputDir :: FilePath
outputDir = "/tmp/malgo_test"

main :: IO ()
main =
  hspec do
    -- Setup directory for test
    runIO setupTestDir
    -- Setup malgo base library
    runIO do
      setupRuntime
      setupBuiltin
      setupPrelude
    testcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory testcaseDir
    describe "Test malgo to-ll" $ parallel do
      for_ testcases \testcase -> do
        describe testcase do
          it ("test usual case " <> testcase) $ example do
            testNormal (testcaseDir </> testcase)
          it ("test nono case " <> testcase <> " (no optimization, no lambda-lifting)") $ example do
            testNoNo (testcaseDir </> testcase)
          it ("test noopt case " <> testcase <> " (no optimization)") $ example do
            testNoOpt (testcaseDir </> testcase)
          it ("test nolift case " <> testcase <> " (no lambda-lift)") $ example do
            testNoLift (testcaseDir </> testcase)
    examples <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory "./examples/malgo"
    describe "Test example malgo to-ll" $ parallel do
      for_ examples \examplecase -> do
        it ("test " <> examplecase) $ example do
          testNormal ("./examples/malgo" </> examplecase)
    errorcases <- runIO $ filter (isExtensionOf "mlg") <$> listDirectory (testcaseDir </> "error")
    describe "Test malgo to-ll (must be error)" $ parallel do
      for_ errorcases \errorcase -> do
        it ("test error case " <> errorcase) $
          testError (testcaseDir </> "error" </> errorcase) `shouldThrow` anyException

#ifdef TEST_ALL
    describe "Test malgo to-ll on all combinations of optimization options" do
      for_ testcases \testcase -> parallel $
        describe testcase do
          for_ optimizeOptions \option -> do
            it ("test " <> testcase <> " " <> show (showOptimizeOption option)) $ example do
              test (testcaseDir </> testcase) (toString $ Text.intercalate "-" $ showOptimizeOption option) True False option
#endif

    describe "Print LLVM assembly" do
      parallel $ for_ testcases \testcase -> do
        it ("test " <> testcase) $ example do
          test (testcaseDir </> testcase) "print" False True defaultOptimizeOption PrintLLVM

setupTestDir :: IO ()
setupTestDir = do
  -- create /tmp/malgo-test
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True (outputDir </> "libs")

-- | Compile Builtin.mlg and copy it to /tmp/malgo-test/libs
setupBuiltin :: IO ()
setupBuiltin = do
  compile "./runtime/malgo/Builtin.mlg" (outputDir </> "libs/Builtin.ll") [outputDir </> "libs"] False False defaultOptimizeOption LLVM

-- | Compile Prelude.mlg and copy it to /tmp/malgo-test/libs
setupPrelude :: IO ()
setupPrelude = do
  compile "./runtime/malgo/Prelude.mlg" (outputDir </> "libs/Prelude.ll") [outputDir </> "libs"] False False defaultOptimizeOption LLVM

-- | Copy runtime.c to /tmp/malgo-test/libs
setupRuntime :: IO ()
setupRuntime = do
  copyFile "./runtime/malgo/runtime.c" (outputDir </> "libs/runtime.c")

-- | Wrapper of 'Malgo.Driver.compile'
compile :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> OptimizeOption -> CompileMode -> IO ()
compile src dst modPaths lambdaLift noOptimize option compileMode =
  hSilence [stderr] do
    malgoEnv <- newMalgoEnv src modPaths Nothing undefined Nothing Nothing
    malgoEnv <-
      pure
        malgoEnv
          { dstPath = dst,
            _modulePaths = [takeDirectory dst, outputDir </> "libs"],
            lambdaLift,
            noOptimize,
            compileMode,
            optimizeOption = option
          }
    Driver.compile src malgoEnv

    -- Check if the generated Koriel code is valid
    let korielPath = dst -<.> "kor"
    koriel <- decodeUtf8 <$> readFileBS korielPath
    case Koriel.parse korielPath koriel of
      Left err ->
        let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
            diag' = addFile diag korielPath (toString koriel)
         in printDiagnostic stderr True False 4 defaultStyle diag' >> exitFailure
      Right ast -> do
        Koriel.lint =<< Koriel.annotate (ModuleName $ convertString $ takeBaseName src) ast

-- | Get the correct name of `clang`
getClangCommand :: IO String
getClangCommand =
  go ["clang-15", "clang"]
  where
    go [] = error "clang not found"
    go (x : xs) = do
      exitCode <- runProcess (proc "which" [x] & setStdout nullStream & setStderr nullStream)
      case exitCode of
        ExitSuccess -> pure x
        ExitFailure _ -> go xs

test ::
  -- | File path of the test case
  FilePath ->
  -- | Type of the test case
  String ->
  -- | Whether to perform lambda-lifting
  Bool ->
  -- | Whether to perform optimization
  Bool ->
  -- | Optimization option
  OptimizeOption ->
  -- | Compile mode
  CompileMode ->
  IO ()
test testcase typ lambdaLift noOptimize option compileMode = do
  createDirectoryIfMissing True (outputDir </> typ)
  let llPath = outputDir </> typ </> takeBaseName testcase -<.> ".ll"
  timeoutWrapper "compile" $
    compile testcase llPath [outputDir </> "libs"] lambdaLift noOptimize option compileMode

  pkgConfig <- map toString . words . decodeUtf8 <$> readProcessStdout_ (proc "pkg-config" ["bdw-gc", "--libs", "--cflags"])
  clang <- getClangCommand
  (exitCode, err) <-
    readProcessStderr
      ( proc
          clang
          $ [ "-Wno-override-module",
              "-lm"
            ]
            <> pkgConfig
            <> [ outputDir </> "libs" </> "runtime.c",
                 outputDir </> typ </> takeBaseName testcase -<.> ".ll",
                 "-o",
                 outputDir </> typ </> takeBaseName testcase -<.> ".out"
               ]
      )
  hPutStr stderr $ convertString err
  when (exitCode /= ExitSuccess) do
    Text.hPutStrLn stderr $ "Exit code: " <> show exitCode
    exitFailure
  (exitCode, result) <-
    timeoutWrapper "run" $
      second decodeUtf8
        <$> readProcessStdout
          ( proc (outputDir </> typ </> takeBaseName testcase -<.> ".out") []
              & setStdin (byteStringInput "Hello")
          )
  when (exitCode /= ExitSuccess) do
    Text.hPutStrLn stderr $ "Exit code: " <> show exitCode
    exitFailure
  expected <- filter ("-- Expected: " `Text.isPrefixOf`) . lines . decodeUtf8 <$> readFileBS testcase
  if map ("-- Expected: " <>) (lines $ Text.stripEnd result) == expected
    then pass
    else do
      Text.hPutStrLn stderr $ "Expected: " <> Text.concat expected
      Text.hPutStrLn stderr $ "Actual: " <> result
      exitFailure
  where
    timeoutWrapper phase m = do
      timeout 60 m >>= \case
        Just x -> pure x
        Nothing -> error $ "timeout in " <> phase

testError :: FilePath -> IO ()
testError testcase = do
  compile testcase (outputDir </> takeBaseName testcase -<.> ".ll") [outputDir </> "libs"] False False defaultOptimizeOption LLVM

testNormal :: FilePath -> IO ()
testNormal testcase = test testcase "normal" True False defaultOptimizeOption LLVM

testNoLift :: FilePath -> IO ()
testNoLift testcase = test testcase "nolift" False False defaultOptimizeOption LLVM

testNoOpt :: FilePath -> IO ()
testNoOpt testcase = test testcase "noopt" True True defaultOptimizeOption LLVM

testNoNo :: FilePath -> IO ()
testNoNo testcase = test testcase "nono" False True defaultOptimizeOption LLVM

#ifdef TEST_ALL
showOptimizeOption :: OptimizeOption -> [Text]
showOptimizeOption OptimizeOption {..} =
  ["fold-variable" | doFoldVariable]
    <> ["inline-constructor" | doInlineConstructor]
    <> ["eliminate-unused-let" | doEliminateUnusedLet]
    <> ["inline-function" | doInlineFunction]
    <> ["fold-redudant-cast" | doFoldRedundantCast]
    <> ["fold-trivial-call" | doFoldTrivialCall]
    <> ["specialize-function" | doSpecializeFunction]

optimizeOptions :: [OptimizeOption]
optimizeOptions =
  let inlineThreshold = 10
      doSpecializeFunction = False
   in [ OptimizeOption {..}
        | doFoldVariable <- [True, False],
          doInlineConstructor <- [True, False],
          doEliminateUnusedLet <- [True, False],
          doInlineFunction<- [True, False],
          doFoldRedundantCast <- [True, False],
          doFoldTrivialCall <- [True, False]
      ]
#endif

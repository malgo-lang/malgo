module Main (main) where

import Criterion
import Criterion.Main
import Data.String.Conversions
import Effectful
import Malgo.Core.Optimize (defaultOptimizeOption)
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, (-<.>), (</>))
import System.Process.Typed (ExitCode (..), nullStream, proc, readProcessStderr_, readProcessStdout_, runProcess, runProcess_, setStderr, setStdout)

benchmarkDir :: FilePath
benchmarkDir = "benchmarks"

outputDir :: FilePath
outputDir = "/tmp/malgo_benchmark"

setupEnv :: IO [FilePath]
setupEnv = do
  setupOutputDir
  setupBuiltin
  setupPrelude
  setupRuntime
  benchmarks <- filter (isExtensionOf "mlg") <$> listDirectory benchmarkDir
  for_ benchmarks \benchmark -> do
    let llOptPath = outputDir </> benchmark -<.> "ll"
    let llNoOptPath = outputDir </> benchmark -<.> "no-opt.ll"
    let llNoLLPath = outputDir </> benchmark -<.> "no-ll.ll"
    compile (benchmarkDir </> benchmark) llOptPath True False
    compile (benchmarkDir </> benchmark) llNoOptPath True True
    compile (benchmarkDir </> benchmark) llNoLLPath False False
    runClang llOptPath
    runClang llNoOptPath
    runClang llNoLLPath
  pure benchmarks
  where
    runClang llPath = do
      pkgConfig <- words . convertString <$> readProcessStdout_ (proc "pkg-config" ["bdw-gc", "--libs", "--cflags"])
      clang <- getClangCommand
      err <-
        readProcessStderr_
          ( proc
              clang
              $ [ "-Wno-override-module",
                  "-lm",
                  "-Xclang",
                  "-opaque-pointers"
                ]
              <> pkgConfig
              <> [ outputDir </> "libs" </> "runtime.c",
                   llPath,
                   "-o",
                   llPath -<.> "out"
                 ]
          )
      hPutStr stderr $ convertString err

main :: IO ()
main = do
  benchmarks <- setupEnv
  defaultMain
    [ bgroup "optimized" $ map runOpt benchmarks,
      bgroup "no optimized" $ map runNoOpt benchmarks,
      bgroup "no lambdalift" $ map runNoLL benchmarks
    ]
  where
    runOpt testcase = bench (takeBaseName testcase) $ nfIO $ do
      runProcess_
        $ proc (outputDir </> takeBaseName testcase -<.> "out") []
        & setStdout nullStream
    runNoOpt testcase = bench (takeBaseName testcase) $ nfIO $ do
      runProcess_
        $ proc (outputDir </> takeBaseName testcase -<.> "no-opt.out") []
        & setStdout nullStream
    runNoLL testcase = bench (takeBaseName testcase) $ nfIO $ do
      runProcess_
        $ proc (outputDir </> takeBaseName testcase -<.> "no-ll.out") []
        & setStdout nullStream

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

setupOutputDir :: IO ()
setupOutputDir = do
  -- create outputDir
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True (outputDir </> "libs")

setupBuiltin :: IO ()
setupBuiltin = do
  compile "./runtime/malgo/Builtin.mlg" (outputDir </> "libs/Builtin.ll") False False

setupPrelude :: IO ()
setupPrelude = do
  compile "./runtime/malgo/Prelude.mlg" (outputDir </> "libs/Prelude.ll") False False

setupRuntime :: IO ()
setupRuntime = do
  -- setCurrentDirectory "./griff"
  -- runProcess_ $ proc "cargo" ["build", "--release"]
  -- setCurrentDirectory "../"
  -- copyFile "./griff/target/release/libgriff_rustlib.a" (outputDir </> "libs/libgriff_rustlib.a")
  copyFile "./runtime/malgo/runtime.c" (outputDir </> "libs/runtime.c")

-- | Wrapper of 'Malgo.Driver.compile'
compile :: FilePath -> FilePath -> Bool -> Bool -> IO ()
compile src dst lambdaLift noOptimize =
  runEff
    $ runMalgoM dst LLVM Flag {lambdaLift, noOptimize, debugMode = False, testMode = False} defaultOptimizeOption
    $ Driver.compile src

module Main (main) where

import Criterion
import Criterion.Main
import Data.String.Conversions
import Koriel.Id (ModuleName (..))
import Malgo.Driver qualified as Driver
import Malgo.Monad (MalgoEnv (..), newMalgoEnv)
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.FilePath (isExtensionOf, takeBaseName, takeDirectory, (-<.>), (</>))
import System.IO (hPutStr)
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
    compile (benchmarkDir </> benchmark) llOptPath [outputDir </> "libs"] True False
    compile (benchmarkDir </> benchmark) llNoOptPath [outputDir </> "libs"] True True
    compile (benchmarkDir </> benchmark) llNoLLPath [outputDir </> "libs"] False False
    runClang llOptPath
    runClang llNoOptPath
    runClang llNoLLPath
  pure benchmarks
  where
    runClang llPath = do
      pkgConfig <- map toString . words . decodeUtf8 <$> readProcessStdout_ (proc "pkg-config" ["bdw-gc", "--libs", "--cflags"])
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
  compile "./runtime/malgo/Builtin.mlg" (outputDir </> "libs/Builtin.ll") [outputDir </> "libs"] False False

setupPrelude :: IO ()
setupPrelude = do
  compile "./runtime/malgo/Prelude.mlg" (outputDir </> "libs/Prelude.ll") [outputDir </> "libs"] False False

setupRuntime :: IO ()
setupRuntime = do
  -- setCurrentDirectory "./griff"
  -- runProcess_ $ proc "cargo" ["build", "--release"]
  -- setCurrentDirectory "../"
  -- copyFile "./griff/target/release/libgriff_rustlib.a" (outputDir </> "libs/libgriff_rustlib.a")
  copyFile "./runtime/malgo/runtime.c" (outputDir </> "libs/runtime.c")

-- | Wrapper of 'Malgo.Driver.compile'
compile :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
compile src dst modPaths lambdaLift noOptimize = do
  malgoEnv <- newMalgoEnv src modPaths Nothing (ModuleName "tmp") Nothing Nothing
  malgoEnv <-
    pure
      malgoEnv
        { dstPath = dst,
          _modulePaths = takeDirectory dst : malgoEnv._modulePaths,
          lambdaLift,
          noOptimize
        }
  Driver.compile src malgoEnv

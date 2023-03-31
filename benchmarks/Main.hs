{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Data.String.Conversions (convertString)
import Malgo.Driver qualified as Driver
import Malgo.Monad (MalgoEnv (..), newMalgoEnv)
import Relude
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory, setCurrentDirectory)
import System.FilePath (isExtensionOf, takeDirectory, (-<.>), (</>))
import System.IO (hPutStr)
import System.Process.Typed (ExitCode (..), nullStream, proc, readProcessStderr_, readProcessStdout_, runProcess, runProcess_, setStderr, setStdout)

benchmarkDir :: FilePath
benchmarkDir = "benchmarks"

outputDir :: FilePath
outputDir = "/tmp/malgo_benchmark"

main :: IO ()
main = do
  setupOutputDir
  setupBuiltin
  setupPrelude
  setupRuntime
  benchmarks <- filter (isExtensionOf "mlg") <$> listDirectory benchmarkDir
  for_ benchmarks \benchmark -> do
    let llPath = outputDir </> benchmark -<.> "ll"
    compile (benchmarkDir </> benchmark) llPath [outputDir </> "libs"] False False
    pkgConfig <- map toString . words . decodeUtf8 <$> readProcessStdout_ (proc "pkg-config" ["bdw-gc", "--libs", "--cflags"])
    clang <- getClangCommand
    err <-
      readProcessStderr_
        ( proc
            clang
            $ [ "-Wno-override-module",
                "-lm"
              ]
              <> pkgConfig
              <> [ outputDir </> "libs" </> "runtime.c",
                   llPath,
                   outputDir </> "libs" </> "libgriff_rustlib.a",
                   "-lpthread",
                   "-ldl",
                   "-o",
                   llPath -<.> "out"
                 ]
        )
    hPutStr stderr $ convertString err

-- | Get the correct name of `clang`
getClangCommand :: IO String
getClangCommand =
  go ["clang", "clang-12"]
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
  setCurrentDirectory "./griff"
  runProcess_ $ proc "cargo" ["build", "--release"]
  setCurrentDirectory "../"
  copyFile "./griff/target/release/libgriff_rustlib.a" (outputDir </> "libs/libgriff_rustlib.a")
  copyFile "./runtime/malgo/runtime.c" (outputDir </> "libs/runtime.c")

-- | Wrapper of 'Malgo.Driver.compile'
compile :: FilePath -> FilePath -> [FilePath] -> Bool -> Bool -> IO ()
compile src dst modPaths lambdaLift noOptimize = do
  malgoEnv <- newMalgoEnv src modPaths Nothing undefined Nothing Nothing
  malgoEnv <-
    pure
      malgoEnv
        { dstPath = dst,
          _modulePaths = takeDirectory dst : malgoEnv._modulePaths,
          lambdaLift,
          noOptimize
        }
  Driver.compile src malgoEnv
module Malgo.TestUtils
  ( smallIndentNoColor,
    pShowCompact,
    testcaseDir,
    getWorkspaceDir,
    setupRuntime,
    setupBuiltin,
    setupPrelude,
    flag,
    option,
    goldenWithTag,
    goldenWithTag',
    goldenJSON,
    goldenLLVM,
    goldenHaskell,
  )
where

import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Text.Lazy qualified as TL
import Malgo.Core.Optimize (OptimizeOption, defaultOptimizeOption)
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
import System.Directory
import System.FilePath ((<.>), (</>))
import Test.Hspec (Spec, it)
import Test.Hspec.Core.Spec (SpecM, getSpecDescriptionPath)
import Test.Hspec.Golden
import Text.Pretty.Simple

smallIndentNoColor :: OutputOptions
smallIndentNoColor =
  defaultOutputOptionsNoColor
    { outputOptionsIndentAmount = 1,
      outputOptionsCompactParens = True,
      outputOptionsCompact = True
    }

pShowCompact :: (ConvertibleStrings TL.Text b, Show a) => a -> b
pShowCompact x = convertString $ pShowOpt smallIndentNoColor x

testcaseDir :: FilePath
testcaseDir = "./test/testcases/malgo"

getWorkspaceDir :: (MonadIO m) => m FilePath
getWorkspaceDir = liftIO do
  pwd <- getCurrentDirectory
  createDirectoryIfMissing True $ pwd </> ".malgo-work"
  createDirectoryIfMissing True $ pwd </> ".malgo-work" </> "libs"
  return $ pwd </> ".malgo-work"

-- | Copy runtime.c to /tmp/malgo-test/libs
setupRuntime :: IO ()
setupRuntime = do
  workspaceDir <- getWorkspaceDir
  copyFile "./runtime/malgo/runtime.c" (workspaceDir </> "libs/runtime.c")

setupBuiltin :: IO ()
setupBuiltin =
  runMalgoM LLVM flag option do
    Driver.compile "./runtime/malgo/Builtin.mlg"

setupPrelude :: IO ()
setupPrelude =
  runMalgoM LLVM flag option do
    Driver.compile "./runtime/malgo/Prelude.mlg"

flag :: Flag
flag = Flag {noOptimize = False, lambdaLift = False, debugMode = False, testMode = True, exitAfterDesugar = False}

option :: OptimizeOption
option = defaultOptimizeOption

goldenWithTag :: String -> String -> String -> IO BL.ByteString -> Spec
goldenWithTag tag ext description action = do
  path <- (<> words description) <$> getSpecDescriptionPath
  let name = intercalate "-" path
  it (tag <> " " <> description) do
    actualOutput <- action
    pure
      Golden
        { output = convertString actualOutput,
          encodePretty = convertString,
          writeToFile = writeFile,
          readFromFile = readFile,
          goldenFile = ".golden" </> tag </> name </> "golden" <.> ext,
          actualFile = Just (".golden" </> tag </> name </> "actual" <.> ext),
          failFirstTime = False
        }

goldenWithTag' :: (ConvertibleStrings a String) => String -> String -> String -> SpecM a ()
goldenWithTag' tag ext description = do
  path <- (<> words description) <$> getSpecDescriptionPath
  let name = intercalate "-" path
  it (tag <> " " <> description) \actualOutput -> do
    Golden
      { output = convertString actualOutput,
        encodePretty = convertString,
        writeToFile = writeFile,
        readFromFile = readFile,
        goldenFile = ".golden" </> tag </> name </> "golden" <.> ext,
        actualFile = Just (".golden" </> tag </> name </> "actual" <.> ext),
        failFirstTime = False
      }

goldenJSON :: String -> String -> IO BL.ByteString -> Spec
goldenJSON tag = goldenWithTag tag "json"

goldenLLVM :: String -> String -> IO BL.ByteString -> Spec
goldenLLVM tag = goldenWithTag tag "ll"

goldenHaskell :: String -> String -> IO BL.ByteString -> Spec
goldenHaskell tag = goldenWithTag tag ""
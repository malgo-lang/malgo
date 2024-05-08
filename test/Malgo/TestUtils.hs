module Malgo.TestUtils where

import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Text.Lazy qualified as TL
import Malgo.Core.Optimize (OptimizeOption, defaultOptimizeOption)
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
import System.FilePath ((<.>), (</>))
import Test.Hspec (Spec, it)
import Test.Hspec.Core.Spec (getSpecDescriptionPath)
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

goldenJSON :: String -> String -> IO BL.ByteString -> Spec
goldenJSON tag = goldenWithTag tag "json"

goldenLLVM :: String -> String -> IO BL.ByteString -> Spec
goldenLLVM tag = goldenWithTag tag "ll"

goldenHaskell :: String -> String -> IO BL.ByteString -> Spec
goldenHaskell tag = goldenWithTag tag ""
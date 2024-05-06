module Malgo.TestUtils where

import Data.Text.Lazy as TL
import Malgo.Core.Optimize (OptimizeOption, defaultOptimizeOption)
import Malgo.Driver qualified as Driver
import Malgo.Monad
import Malgo.Prelude
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
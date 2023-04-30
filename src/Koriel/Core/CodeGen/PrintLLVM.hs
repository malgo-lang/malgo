module Koriel.Core.CodeGen.PrintLLVM (codeGen) where

import Koriel.Core.Syntax (Program (..))
import Koriel.Id (ModuleName)
import Koriel.Prelude
import Koriel.Pretty (pPrint)

-- | Generate and Write LLVM IR to file from a Koriel module.
codeGen ::
  -- | Source path
  FilePath ->
  -- | Destination path
  FilePath ->
  ModuleName ->
  Program a ->
  IO ()
codeGen srcPath dstPath moduleName Program {} = do
  withFile dstPath WriteMode \handle -> do
    hPutTextLn handle $ "source_filename = \"" <> toText srcPath <> "\""
    hPutTextLn handle "define i32 @main() {"
    hPutTextLn handle "\tret i32 0"
    hPutTextLn handle "}"

data CodeGenEnv = CodeGenEnv
  { moduleName :: ModuleName,
    output :: Handle
  }
  deriving stock (Show, Eq, Generic)
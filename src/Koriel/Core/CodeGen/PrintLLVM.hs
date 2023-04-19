module Koriel.Core.CodeGen.PrintLLVM (codeGen) where

import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude
import Malgo.Monad

-- | Generate LLVM IR from a program.
codeGen ::
  MonadIO m =>
  -- | The path of the source file.
  FilePath ->
  -- | Malgo environment.
  MalgoEnv ->
  -- | The name of the module.
  ModuleName ->
  -- | Program.
  Program (Id Type) ->
  m ()
codeGen srcPath malgoEnv modName Program {..} = do
  writeFile malgoEnv.dstPath ""

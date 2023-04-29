module Koriel.Core.CodeGen.PrintLLVM (codeGen) where

import Koriel.Core.Syntax (Program (..))
import Koriel.Prelude

-- | Generate and Write LLVM IR to file from a Koriel module.
codeGen :: (MonadIO m, Show a) => Program a -> m ()
codeGen Program {..} = do
  putStrLn "Generating LLVM IR..."
  print extFuns
  print topVars
  print topFuns
  putStrLn "Done."
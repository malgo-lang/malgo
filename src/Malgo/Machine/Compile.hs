-- | Compile Malgo AST to Malgo Machine Code.
module Malgo.Machine.Compile (compile) where

import Effectful
import Malgo.Infer.TcEnv (TcEnv)
import Malgo.Machine.Command
import Malgo.Prelude
import Malgo.Syntax as S
import Malgo.Syntax.Extension as S

compile :: () => TcEnv -> Module (Malgo 'Refine) -> Eff es ()
compile = undefined
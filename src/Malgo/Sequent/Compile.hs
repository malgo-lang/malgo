-- | Compile Malgo AST to Malgo Sequent Code.
module Malgo.Sequent.Compile (compile) where

import Effectful
import Malgo.Infer.TcEnv (TcEnv)
import Malgo.Prelude
import Malgo.Syntax as S
import Malgo.Syntax.Extension as S

compile :: () => TcEnv -> Module (Malgo 'Refine) -> Eff es ()
compile = undefined

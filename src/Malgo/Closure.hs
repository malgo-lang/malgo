module Malgo.Closure (ClosurePass(..)) where

import Data.Void
import Malgo.Pass
import Malgo.Syntax (Module (..))
import Malgo.Syntax.Extension

-- | Dummy closure conversion pass. Currently it performs no transformation.
data ClosurePass = ClosurePass

instance Pass ClosurePass where
  type Input ClosurePass = Module (Malgo Rename)
  type Output ClosurePass = Module (Malgo Closure)
  type ErrorType ClosurePass = Void
  type Effects ClosurePass es = ()
  runPassImpl _ m = pure $ unsafeCoerceModule m

-- | Convert between modules of different phases by reusing the underlying data.
unsafeCoerceModule :: Module (Malgo a) -> Module (Malgo b)
unsafeCoerceModule (Module name defs) = Module name defs

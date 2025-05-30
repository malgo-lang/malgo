-- | Malgo.Pass provides a pass management system for the Malgo compiler.
-- It allows for the registration and execution of various compiler passes, such as renaming, type inference, and refinement in a modular way.
module Malgo.Pass (Pass (..)) where

import Data.Kind (Constraint)
import Effectful

-- | `Pass` representing a compiler pass.
class Pass pass where
  -- | `Input` is the data type that the pass comsumes.
  type Input pass

  -- | `Output` is the data type that the pass produces.
  type Output pass

  type Effects pass (es :: [Effect]) :: Constraint

  -- | `run` executes the pass on the given input and returns the output.
  runPass :: (Effects pass es) => pass -> Input pass -> Eff es (Output pass)
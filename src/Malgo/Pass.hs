-- | Malgo.Pass provides a pass management system for the Malgo compiler.
-- It allows for the registration and execution of various compiler passes, such as renaming, type inference, and refinement in a modular way.
module Malgo.Pass (Pass (..), CompileError) where

import Data.Either (Either (..))
import Data.Kind (Constraint)
import Effectful
import Effectful.Error.Static (CallStack, Error, prettyCallStack, runError, throwError)
import Prelude (Show, pure, show, (<>))

-- | CompileError wraps any error with its call stack for uniform error handling.
data CompileError = forall e. (Show e) => CompileError {callStack :: CallStack, compileError :: e}

instance Show CompileError where
  show (CompileError {callStack, compileError}) =
    prettyCallStack callStack <> "\n" <> show compileError

-- | wrapCompileError runs an Eff computation that may throw an compile error, and wraps it as CompileError.
wrapCompileError :: (Show e, Error CompileError :> es) => Eff (Error e : es) a -> Eff es a
wrapCompileError m = do
  result <- runError m
  case result of
    Left (callStack, error) -> throwError (CompileError {callStack, compileError = error})
    Right x -> pure x

class (Show (ErrorType pass)) => Pass pass where
  type Input pass
  type Output pass

  -- | ErrorType is the type of error that the pass can produce.
  type ErrorType pass

  -- | Effects is a set of effects that the pass can use.
  -- It should not include the `Error (ErrorType pass)` effect, as it is handled by the runPass function.
  type Effects pass (es :: [Effect]) :: Constraint

  -- | runPass executes the pass on the given input and returns the output, wrapping errors as CompileError by default.
  runPass :: (Effects pass es, Error CompileError :> es) => pass -> Input pass -> Eff es (Output pass)
  runPass pass input = wrapCompileError (runPassImpl pass input)

  -- | runPassImpl should be implemented by each pass, returning Eff (Error (ErrorType pass) : es) (Output pass)
  runPassImpl :: (Effects pass es) => pass -> Input pass -> Eff (Error (ErrorType pass) : es) (Output pass)

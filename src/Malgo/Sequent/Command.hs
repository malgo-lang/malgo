module Malgo.Sequent.Command
  ( Program (..),
    Branch (..),
    Command (..),
    Code
  )
where

import Malgo.Prelude
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data Program = Program
  {definitions :: [(Range, Name, Code)]}
  deriving stock (Show)

type Code = [Command]

-- | Command is a basic unit of computation.
data Command
  = -- Producer

    -- | @(S, E, Fetch(name) : C) -> (E[name] : S, E, C)@
    Fetch Range Name
  | -- | Push pushes a literal as a value.
    --
    -- @(S, E, Push(lit) : C) -> (lit : S, E, C)@
    Push Range Literal
  | -- | Construct creates a new structure with tag and push it to the stack.
    --
    -- @(value*n <> S, E, Construct(tag, n) : C) -> (tag(value*n) : S, E, C)@
    Construct Range Tag Int
  | -- | Lambda create a new (recursive) closure and push it to the stack.
    --
    -- @(S, E, Lambda(name*, code) : C) -> (λ E name* . code : S, E, C)@
    Lambda Range [Name] Code
  | -- | Object creates a new object and push it to the stack.
    -- Unlike other languages, Object's fields are lazy.
    --
    -- @(S, E, Object(field*) : C) -> ({E field*} : S, E, C)@
    Object Range (Map Text Code)
  | -- | Do captures rest of the code and switch to the new block.
    --
    -- @(S, E, Do(name, code) : C) -> (S, E {name = C}, code)@
    Do Range Name Code
  | -- | Suspend creates a scoped label.
    -- In other words, it turns a consumer into a producer.
    --
    -- @(S, E, Suspend(code) : C) -> ([E, code] : S, E, C)@
    Suspend [Command]
  | -- Consumer

    -- | Resume jumps to a scoped label created by Suspend.
    --
    -- @(S, E { name = [E', code] }, Resume(name))) -> (S, E', code)@
    Resume Range Name
  | -- | Apply applies a function to the top of the stack.
    --
    -- @(value*n <> λ E name* . code : S, _, Apply(n)) -> (S, E {name* = reverse value*n}, code)@
    Apply Range Int
  | -- | Proj projects a field from the top of the stack.
    --
    -- @({E field: code, ...} : S, _, Proj(field, return)) -> (S, E, code : return)@
    Proj Range Text
  | -- | Then assigns a value to a variable.
    --
    -- @(value : S, E, Then(name, code)) -> (S, E {name = value}, code)@
    Then Range Name Code
  | -- | Finish finishes the evaluation.
    --
    -- @(value : S, E, Finish) -> exit with value@
    Finish Range
  | -- Statement

    -- | Primitive is a primitive operation.
    --
    -- @(value* <> S, E, Primitive(name)) -> modified S, E, C by the primitive operation@
    Primitive Range Text
  | -- | Select selects a branch based on the values on the stack.
    --
    -- @(value : S, E, Select(range, branch*)) -> (S, E, selected branch)@
    Select Range [Branch]
  deriving stock (Show)

data Branch = Branch
  { range :: Range,
    pattern :: Pattern,
    code :: Code
  }
  deriving stock (Show)
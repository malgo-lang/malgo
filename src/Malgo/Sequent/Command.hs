module Malgo.Sequent.Command
  ( Program (..),
    Branch (..),
    Command (..),
    Code,
  )
where

import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Malgo.Prelude
import Malgo.SExpr (ToSExpr (toSExpr))
import Malgo.SExpr qualified as S
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data Program = Program
  {definitions :: [(Range, Name, Code)]}
  deriving stock (Show)

instance ToSExpr Program where
  toSExpr (Program defs) = S.L $ map (\(_, name, body) -> toSExpr (name, body)) defs

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

instance ToSExpr Command where
  toSExpr (Fetch _ name) = S.L [S.A "fetch", toSExpr name]
  toSExpr (Push _ lit) = S.L [S.A "push", toSExpr lit]
  toSExpr (Construct _ tag n) = S.L [S.A "construct", toSExpr tag, S.A $ S.Int (fromIntegral n) Nothing]
  toSExpr (Lambda _ names code) = S.L [S.A "lambda", S.L $ map toSExpr names, toSExpr code]
  toSExpr (Object _ fields) = S.L [S.A "object", S.L $ map (\(k, v) -> S.L [toSExpr k, toSExpr v]) $ Map.toList fields]
  toSExpr (Do _ name code) = S.L [S.A "do", toSExpr name, toSExpr code]
  toSExpr (Suspend code) = S.L [S.A "suspend", S.L $ map toSExpr code]
  toSExpr (Resume _ name) = S.L [S.A "resume", toSExpr name]
  toSExpr (Apply _ n) = S.L [S.A "apply", S.A $ S.Int (fromIntegral n) Nothing]
  toSExpr (Proj _ field) = S.L [S.A "proj", toSExpr field]
  toSExpr (Then _ name code) = S.L [S.A "then", toSExpr name, toSExpr code]
  toSExpr (Finish _) = S.A "finish"
  toSExpr (Primitive _ name) = S.L [S.A "primitive", toSExpr name]
  toSExpr (Select _ branches) = S.L $ S.A "select" : map toSExpr branches

data Branch = Branch
  { range :: Range,
    pattern :: Pattern,
    code :: Code
  }
  deriving stock (Show)

instance ToSExpr Branch where
  toSExpr (Branch _ pattern code) = S.L [toSExpr pattern, toSExpr code]
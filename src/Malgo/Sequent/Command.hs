module Malgo.Sequent.Command
  ( Program (..),
    Branch (..),
    Command (..),
    Code,
    lintProgram,
    LintError (..),
  )
where

import Control.Lens (traverseOf_, _2)
import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static (CallStack, Error, runError, throwError)
import Effectful.Reader.Static (Reader, local, runReader)
import Malgo.Prelude hiding (throwError)
import Malgo.SExpr (ToSExpr (toSExpr))
import Malgo.SExpr qualified as S
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data Program = Program
  {definitions :: [(Range, Name, Name, Code)]}
  deriving stock (Show)

instance ToSExpr Program where
  toSExpr (Program defs) = S.L $ map (\(_, name, return, body) -> S.L $ toSExpr name : toSExpr return : map toSExpr body) defs

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
    Object Range (Map Text (Name, Code))
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
  | -- | Assign assigns a value to a variable.
    --
    -- @(value : S, E, Assign(name) : C) -> (S, E {name = value}, C)@
    Assign Range Name
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
  | -- | Invoke invokes a toplevel definition.
    --
    -- @(cont : S, E {name = μ return code}, Invoke(name)) -> (S, E {return = cont}, code)@
    Invoke Range Name
  deriving stock (Show)

instance ToSExpr Command where
  toSExpr (Fetch _ name) = S.L [S.A "fetch", toSExpr name]
  toSExpr (Push _ lit) = S.L [S.A "push", toSExpr lit]
  toSExpr (Construct _ tag n) = S.L [S.A "construct", toSExpr tag, S.A $ S.Int (fromIntegral n) Nothing]
  toSExpr (Lambda _ names code) = S.L $ [S.A "lambda", toSExpr names] <> map toSExpr code
  toSExpr (Object _ fields) = S.L [S.A "object", S.L $ map (\(k, v) -> S.L $ toSExpr k : map toSExpr v) $ Map.toList fields]
  toSExpr (Do _ name code) = S.L $ [S.A "do", toSExpr name] <> map toSExpr code
  toSExpr (Suspend code) = S.L $ S.A "suspend" : map toSExpr code
  toSExpr (Resume _ name) = S.L [S.A "resume", toSExpr name]
  toSExpr (Apply _ n) = S.L [S.A "apply", S.A $ S.Int (fromIntegral n) Nothing]
  toSExpr (Proj _ field) = S.L [S.A "proj", toSExpr field]
  toSExpr (Assign _ name) = S.L [S.A "assign", toSExpr name]
  toSExpr (Finish _) = S.A "finish"
  toSExpr (Primitive _ name) = S.L [S.A "primitive", toSExpr name]
  toSExpr (Select _ branches) = S.L $ S.A "select" : map toSExpr branches
  toSExpr (Invoke _ name) = S.L [S.A "invoke", toSExpr name]

data Branch = Branch
  { range :: Range,
    pattern :: Pattern,
    code :: Code
  }
  deriving stock (Show)

instance ToSExpr Branch where
  toSExpr (Branch _ pattern code) = S.L $ toSExpr pattern : map toSExpr code

lintProgram :: Program -> Eff es (Either (CallStack, LintError) ())
lintProgram Program {definitions} = do
  let toplevels = Set.fromList $ map (\(_, name, _, _) -> name) definitions
  runError $ runReader toplevels $ traverse_ lintDefinition definitions

lintDefinition :: (Reader (Set Name) :> es, Error LintError :> es) => (Range, Name, Name, Code) -> Eff es ()
lintDefinition (_, _, return, code) = do
  local (Set.insert return) $ lintCode code

data LintError
  = UnexpectedEndOfCode Command
  | UnexpectedContinueOfCode Command
  deriving stock (Show)

lintCode :: (Reader (Set Name) :> es, Error LintError :> es) => [Command] -> Eff es ()
lintCode [] = pure ()
lintCode (cmd@(Fetch _ _) : code) = do
  when (null code) $ throwError $ UnexpectedEndOfCode cmd
  lintCode code
lintCode (cmd@(Push _ _) : code) = do
  when (null code) $ throwError $ UnexpectedEndOfCode cmd
  lintCode code
lintCode (cmd@(Construct _ _ _) : code) = do
  when (null code) $ throwError $ UnexpectedEndOfCode cmd
  lintCode code
lintCode (cmd@(Lambda _ _ body) : code) = do
  lintCode body
  when (null code) $ throwError $ UnexpectedEndOfCode cmd
  lintCode code
lintCode (cmd@(Object _ fields) : code) = do
  traverseOf_ (traverse . _2) lintCode fields
  when (null code) $ throwError $ UnexpectedEndOfCode cmd
  lintCode code
lintCode (cmd@(Do _ _ body) : code) = do
  lintCode body
  when (null code) $ throwError $ UnexpectedEndOfCode cmd
  lintCode code
lintCode (cmd@(Suspend body) : code) = do
  lintCode body
  when (null code) $ throwError $ UnexpectedEndOfCode cmd
  lintCode code
lintCode (cmd@(Resume _ _) : code) = do
  unless (null code) $ throwError $ UnexpectedContinueOfCode cmd
lintCode (cmd@(Apply _ _) : code) = do
  unless (null code) $ throwError $ UnexpectedContinueOfCode cmd
lintCode (cmd@(Proj _ _) : code) = do
  unless (null code) $ throwError $ UnexpectedContinueOfCode cmd
lintCode (cmd@(Assign _ _) : code) = do
  lintCode code
  when (null code) $ throwError $ UnexpectedEndOfCode cmd
lintCode (cmd@(Finish _) : code) = do
  unless (null code) $ throwError $ UnexpectedContinueOfCode cmd
lintCode (cmd@(Primitive _ _) : code) = do
  unless (null code) $ throwError $ UnexpectedContinueOfCode cmd
lintCode (cmd@(Select _ branches) : code) = do
  for_ branches $ \Branch {code} -> lintCode code
  unless (null code) $ throwError $ UnexpectedContinueOfCode cmd
lintCode (cmd@(Invoke _ _) : code) = do
  unless (null code) $ throwError $ UnexpectedContinueOfCode cmd
module Malgo.Machine.Command
  ( Program (..),
    Branch (..),
    Tag (..),
    Literal (..),
    Command (..),
    Pattern (..),
    Name,
  )
where

import Malgo.Id
import Malgo.Prelude

data Program = Program
  {definitions :: [(Name, Code)]}

type Code = [Command]

data Branch = Branch
  { range :: Range,
    patterns :: [Pattern],
    code :: Code
  }

-- | Command is a basic unit of computation.
data Command where
  -- Producer

  -- | Push pushes a literal as a value.
  Push :: Range -> Literal -> Command
  Get :: Range -> Name -> Command
  -- | Construct creates a new structure with tag and push it to the stack.
  Construct ::
    Range ->
    Tag ->
    -- | Number of fields.
    Int ->
    Command
  -- | Lambda create a new (recursive) closure and push it to the stack.
  Lambda :: Range -> Name -> Code -> Command
  -- | Object creates a new object and push it to the stack.
  -- Unlike other languages, Object's fields are lazy.
  Object :: Range -> Map Text Code -> Command
  Do :: Range -> Name -> Code -> Command
  -- Consumer

  -- | Apply applies a function to the top of the stack.
  Apply :: Range -> Int -> Int -> Command
  -- | Proj projects a field from the top of the stack.
  Proj :: Range -> Text -> Command
  -- | Assign assigns a value to a variable.
  Then :: Range -> Name -> Command
  -- Statement

  -- | Primitive is a primitive operation.
  Primitive :: Range -> Text -> Command
  -- | Select selects a branch based on the values on the stack.
  Select ::
    Range ->
    [Branch] ->
    Command

-- | Tag is used to distinguish different structures.
data Tag = Tuple | Tag Text

data Literal where
  Int32 :: Int32 -> Literal
  Int64 :: Int64 -> Literal
  Float :: Float -> Literal
  Double :: Double -> Literal
  Char :: Char -> Literal
  String :: Text -> Literal

type Name = Id

data Pattern where
  Var :: Name -> Pattern
  Literal :: Literal -> Pattern
  -- | Destruct a structure.
  Destruct :: Tag -> [Pattern] -> Pattern
  -- | Expand an object.
  Expand :: Map Text Pattern -> Pattern
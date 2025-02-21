{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Malgo.Sequent.Fun
  ( Name,
    Tag (..),
    Literal (..),
    Program (..),
    Expr (..),
    Branch (..),
    Pattern (..),
  )
where

import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Malgo.Id
import Malgo.Prelude
import Malgo.SExpr (ToSExpr (..))
import Malgo.SExpr qualified as S

type Name = Id

-- | Tag is used to distinguish different structures.
data Tag = Tuple | Tag Text
  deriving stock (Show)

instance ToSExpr Tag where
  toSExpr Tuple = S.A "tuple"
  toSExpr (Tag t) = toSExpr t

data Literal
  = Int32 Int32
  | Int64 Int64
  | Float Float
  | Double Double
  | Char Char
  | String Text
  deriving stock (Show)

instance ToSExpr Literal where
  toSExpr (Int32 n) = S.A $ S.Int (fromIntegral n) (Just "i32")
  toSExpr (Int64 n) = S.A $ S.Int (fromIntegral n) (Just "i64")
  toSExpr (Float n) = S.A $ S.Float n
  toSExpr (Double n) = S.A $ S.Double n
  toSExpr (Char c) = S.A $ S.Char c
  toSExpr (String t) = S.A $ S.String t

data Program = Program
  {definitions :: [(Name, [Name], Expr)]}

instance ToSExpr Program where
  toSExpr Program {definitions} = S.L $ map toSExpr definitions

data Expr
  = Var {range :: Range, name :: Name}
  | Literal {range :: Range, literal :: Literal}
  | Construct {range :: Range, tag :: Tag, arguments :: [Expr]}
  | Lambda {range :: Range, parameters :: [Name], body :: Expr}
  | Object {range :: Range, fields :: Map Text Expr}
  | Apply {range :: Range, callee :: Expr, arguments :: [Expr]}
  | Project {range :: Range, callee :: Expr, field :: Text}
  | Primitive {range :: Range, operator :: Text, arguments :: [Expr]}
  | Select {range :: Range, scrutinees :: Expr, branches :: [Branch]}
  deriving stock (Show)

instance ToSExpr Expr where
  toSExpr Var {name} = toSExpr name
  toSExpr Literal {literal} = toSExpr literal
  toSExpr Construct {tag, arguments} = S.L [toSExpr tag, S.L $ map toSExpr arguments]
  toSExpr Lambda {parameters, body} = S.L [S.A "lambda", S.L $ map toSExpr parameters, toSExpr body]
  toSExpr Object {fields} = S.L [S.A "object", S.L $ map toSExpr $ Map.toList fields]
  toSExpr Apply {callee, arguments} = S.L [S.A "apply", toSExpr callee, S.L $ map toSExpr arguments]
  toSExpr Project {callee, field} = S.L [S.A "project", toSExpr callee, toSExpr field]
  toSExpr Primitive {operator, arguments} = S.L [S.A "primitive", toSExpr operator, S.L $ map toSExpr arguments]
  toSExpr Select {scrutinees, branches} = S.L [S.A "select", toSExpr scrutinees, S.L $ map toSExpr branches]

data Branch = Branch {range :: Range, pattern :: Pattern, body :: Expr}
  deriving stock (Show)

instance ToSExpr Branch where
  toSExpr Branch {pattern, body} = S.L [toSExpr pattern, toSExpr body]

data Pattern
  = PVar {range :: Range, name :: Name}
  | PLiteral {range :: Range, literal :: Literal}
  | Destruct {range :: Range, tag :: Tag, patterns :: [Pattern]}
  | Expand {range :: Range, fields :: Map Text Pattern}
  deriving stock (Show)

instance ToSExpr Pattern where
  toSExpr PVar {name} = toSExpr name
  toSExpr PLiteral {literal} = toSExpr literal
  toSExpr Destruct {tag, patterns} = S.L [toSExpr tag, S.L $ map toSExpr patterns]
  toSExpr Expand {fields} = S.L [S.A "expand", S.L $ map toSExpr $ Map.toList fields]
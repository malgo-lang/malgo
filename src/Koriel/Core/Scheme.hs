-- | Koriel.Core.Scheme provides two components:
-- 1. Scheme AST
-- 2. Core to Scheme compiler
-- 3. Scheme AST pretty printer
-- 4. Scheme Evaluator
module Koriel.Core.Scheme where

import Data.String.Conversions (ConvertibleStrings (convertString))
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty (Pretty (pPrint))
import Koriel.Pretty qualified as P
import Text.Encoding.Z (zEncodeString)

newtype Identifier = Identifier String
  deriving newtype (Eq, Ord, Show)

instance Pretty Identifier where
  pPrint (Identifier t) = P.text t

fromId :: Id a -> Identifier
fromId id = Identifier $ zEncodeString $ convertString $ idToText id

data Expr
  = Symbol Identifier
  | Literal Literal
  | Call Expr [Expr]
  | Lambda [Identifier] Expr
  | Cond [Clause]
  | LetRec [Binding] Expr
  deriving stock (Eq, Ord, Show)

instance Pretty Expr where
  pPrint (Symbol id) = pPrint id
  pPrint (Literal lit) = pPrint lit
  pPrint (Call f args) = P.parens $ P.hsep $ map pPrint $ f : args
  pPrint (Lambda args body) = P.parens $ P.sep ["lambda", P.parens $ P.hsep $ map pPrint args, pPrint body]
  pPrint (Cond clauses) = P.parens $ P.sep $ "cond" : map pPrint clauses
  pPrint (LetRec bindings body) = P.parens $ P.sep ["letrec", P.parens $ P.hsep $ map pPrint bindings, pPrint body]

data Literal
  = Integer Integer
  | Float Double
  | Char Char
  | String String
  | Boolean Bool
  deriving stock (Eq, Ord, Show)

instance Pretty Literal where
  pPrint (Integer i) = pPrint i
  pPrint (Float f) = pPrint f
  pPrint (Char c) = "#\\" <> P.char c
  pPrint (String s) = P.doubleQuotes $ P.text s
  pPrint (Boolean True) = "#t"
  pPrint (Boolean False) = "#f"

data Clause = Clause {test :: Expr, body :: Expr}
  deriving stock (Eq, Ord, Show)

instance Pretty Clause where
  pPrint Clause {test, body} = P.parens $ P.sep [pPrint test, "=>", pPrint body]

data Binding = Binding {name :: Identifier, value :: Expr}
  deriving stock (Eq, Ord, Show)

instance Pretty Binding where
  pPrint Binding {name, value} = P.parens $ P.sep [pPrint name, pPrint value]

list :: [Expr] -> Expr
list = Call (Symbol (Identifier "list"))
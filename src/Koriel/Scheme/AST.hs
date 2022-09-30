-- | Koriel.Scheme.AST provides two components:
-- 1. Scheme AST
-- 2. Scheme AST pretty printer
module Koriel.Scheme.AST where

import Data.String.Conversions (ConvertibleStrings (convertString))
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty (Pretty (pPrint))
import Koriel.Pretty qualified as P

newtype Identifier = Identifier String
  deriving newtype (Eq, Ord, Show, Semigroup)

instance IsString Identifier where
  fromString = Identifier

instance Pretty Identifier where
  pPrint (Identifier t) = P.text t

fromId :: Id a -> Identifier
fromId id = Identifier $ encode $ convertString $ idToText id
  where
    encode :: String -> String
    encode = concatMap aux
    aux '|' = "\\|"
    aux c = [c]

data Expr
  = Variable Identifier
  | Symbol Identifier
  | Literal Literal
  | Call Expr [Expr]
  | Lambda [Identifier] Expr
  | Cond [Clause]
  | LetRec [Binding] Expr
  | Define Identifier (Maybe [Identifier]) Expr
  | Error String
  deriving stock (Eq, Ord, Show)

instance Pretty Expr where
  pPrint (Variable i) = pPrint i
  pPrint (Symbol id) = "'" <> pPrint id
  pPrint (Literal lit) = pPrint lit
  pPrint (Call f args) = P.parens $ P.hsep $ map pPrint $ f : args
  pPrint (Lambda args body) = P.parens $ P.sep ["lambda", P.parens $ P.hsep $ map pPrint args, pPrint body]
  pPrint (Cond clauses) = P.parens $ P.sep $ "cond" : map pPrint clauses
  pPrint (LetRec bindings body) = P.parens $ P.sep ["letrec", P.parens $ P.hsep $ map pPrint bindings, pPrint body]
  pPrint (Define name Nothing body) = P.parens $ P.sep ["define", pPrint name, pPrint body]
  pPrint (Define name (Just args) body) = P.parens $ P.sep ["define", P.parens $ P.hsep $ map pPrint $ name : args, pPrint body]
  pPrint (Error msg) = P.parens $ P.sep ["error", P.text msg]

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
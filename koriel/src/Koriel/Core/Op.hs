module Koriel.Core.Op where

import Koriel.Prelude
import Koriel.Pretty

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | FAdd
  | FSub
  | FMul
  | FDiv
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
  deriving stock (Eq, Show, Generic, Data)

instance Pretty Op where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Div = "/"
  pretty FAdd = "+."
  pretty FSub = "-."
  pretty FMul = "*."
  pretty FDiv = "/."
  pretty Mod = "%"
  pretty Eq = "=="
  pretty Neq = "<>"
  pretty Lt = "<"
  pretty Gt = ">"
  pretty Le = "<="
  pretty Ge = ">="
  pretty And = "&&"
  pretty Or = "||"

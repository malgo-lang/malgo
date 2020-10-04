{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
  deriving stock (Eq, Show)

instance Pretty Op where
  pPrint Add = "+"
  pPrint Sub = "-"
  pPrint Mul = "*"
  pPrint Div = "/"
  pPrint FAdd = "+."
  pPrint FSub = "-."
  pPrint FMul = "*."
  pPrint FDiv = "/."
  pPrint Mod = "%"
  pPrint Eq = "=="
  pPrint Neq = "<>"
  pPrint Lt = "<"
  pPrint Gt = ">"
  pPrint Le = "<="
  pPrint Ge = ">="
  pPrint And = "&&"
  pPrint Or = "||"

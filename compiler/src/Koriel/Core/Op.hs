{-# LANGUAGE DeriveAnyClass #-}

module Koriel.Core.Op where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
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
  deriving anyclass (Binary, ToJSON, FromJSON)

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
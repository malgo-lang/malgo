{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.IR.MIR where

import           Data.Outputable
import           Language.Malgo.Id
import           Language.Malgo.Pretty
import           Universum             hiding (Type)

type Name = Id

newtype Env = Env (Map Name TypeRep)
  deriving (Eq, Show, Generic, Outputable)

data Def = Def Name [Name] Block
  deriving (Eq, Show, Generic, Outputable)

data Expr = Var Name
          | Const Constant
          | BinOp Op Name Name
          | If Name Block Block
          | Apply Name Name
          | Tuple [Name]
          | Access Name Int
          | Cast Name TypeRep
  deriving (Eq, Show, Generic, Outputable)

data Op = Add | Sub | Mul | Div | Mod
        | FAdd | FSub | FMul | FDiv
        | Eq | Neq | Lt | Gt | Le | Ge
        | And | Or
  deriving (Eq, Show, Generic, Outputable)

instance Pretty Op where
  pPrint op = case op of
    { Add -> "+"; Sub -> "-"; Mul -> "*"; Div -> "/"; Mod -> "%"
    ; FAdd -> "+."; FSub -> "-."; FMul -> "*."; FDiv -> "/."
    ; Eq -> "=="; Neq -> "/="; Lt -> "<"; Gt -> ">"; Le -> "<="; Ge -> ">="
    ; And -> "&"; Or -> "|" }

data Constant = Int Int Integer
              | Float Double
              | String [Int]
  deriving (Eq, Show, Generic, Outputable)

data Inst = Let Name Expr
  deriving (Eq, Show, Generic, Outputable)

data Block = Block { insts :: [Inst], term :: Expr }
  deriving (Eq, Show, Generic, Outputable)

data TypeRep = BoxType
             | IntType Int
             | TupleType [TypeRep]
             | ArrowType TypeRep TypeRep
             | StringType
  deriving (Eq, Show, Generic, Outputable)

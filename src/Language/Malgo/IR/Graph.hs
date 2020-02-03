{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Language.Malgo.IR.Graph where

import           Language.Malgo.Prelude
import           Language.Malgo.TypeRep.LType
import           Language.Malgo.ID

import           Compiler.Hoopl

newtype Program = Program { functions :: [Func] }

data Func = Func { name :: Var, params :: [Var], entry :: Label, body :: Graph Insn C C }

type Var = ID LType

data Insn e x where
  Label :: Label -> Insn C O
  Assign :: Var -> Expr -> Insn O O
  Store :: Var -> Expr -> Expr -> Insn O O
  Branch :: Label -> Insn O C
  Cond :: Expr -> Label -> Label -> Insn O C
  Return :: Expr -> Insn O C

data Expr = Bool Bool
          | Int32 Int32
          | Int64 Int64
          | Word8 Word8
          | Word32 Word32
          | Word64 Word64
          | Float64 Double
          | String String
          | ArrayCreate LType Var
          | Alloca LType
          | Load Var [Var]
          | Call Var [Var]
          | CallExt String LType
          | Cast LType Var
          | Trunc LType Var
          | Zext LType Var
          | Sext LType Var
          | Undef LType
          | Add Var Var
          | Sub Var Var
          | Mul Var Var
          | Sdiv Var Var
          | Srem Var Var
          | Udiv Var Var
          | Urem Var Var
          | Fadd Var Var
          | Fsub Var Var
          | Fmul Var Var
          | Ieq Var Var
          | Ine Var Var
          | Slt Var Var
          | Sgt Var Var
          | Sle Var Var
          | Sge Var Var
          | Ult Var Var
          | Ugt Var Var
          | Ule Var Var
          | Uge Var Var
          | Feq Var Var
          | Fne Var Var
          | Flt Var Var
          | Fgt Var Var
          | Fle Var Var
          | Fge Var Var
          | And Var Var
          | Or Var Var

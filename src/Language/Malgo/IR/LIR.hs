{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Language.Malgo.IR.LIR where

import           Language.Malgo.Pretty
import           Relude                hiding (Op, Type)

data Program a = Program { functions :: [Func a], mainFunc :: a }
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

data Func a = Func { name :: a, params :: [a], body :: Block a }
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

newtype Block a = Block { insts :: [(a, Inst a)] }
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

data Inst a = Var a
            | Const Constant
            | Call a [a]
            | Alloca Type a
            | Load a [a]
            | Store a [a] a
            | Cast Type a
            | Undef Type
            | BinOp Op a a
            | If a (Block a) (Block a)
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

data Constant = Bool Bool
              | Int32 Int32
              | Int64 Int64
              | Word8 Word8
              | Word32 Word32
              | Word64 Word64
  deriving (Eq, Show, Read, Generic, PrettyVal)

data Op = ADD | SUB | MUL | SDIV | SREM | UDIV | UREM
        | FADD | FSUB | FMUL | FDIV
        | IEQ | INE
        | SLT | SGT | SLE | SGE
        | ULT | UGT | ULE | UGE
        | FEQ | FNE
        | FLT | FGT | FLE | FGE
        | AND | OR
  deriving (Eq, Show, Read, Generic, PrettyVal)

data Type = Ptr Type
          | Bit
          | I32
          | I64
          | U8
          | U32
          | U64
          | Struct [Type]
          | Function Type [Type]
  deriving (Eq, Show, Read, Generic, PrettyVal)

pattern Boxed :: Type
pattern Boxed = Ptr U8

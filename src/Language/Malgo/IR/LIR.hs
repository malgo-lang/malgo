{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Language.Malgo.IR.LIR where

import           Language.Malgo.IR.HIR (Lit (..), Op (..))
import           Language.Malgo.Pretty
import           Relude                hiding (Op, Type)

data Program t a = Program { functions :: [Func t a], mainFunc :: a }
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

data Func t a = Func { name :: a, params :: [a], body :: Block t a }
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

newtype Block t a = Block { insts :: [(a, Expr t a)] }
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

data Expr t a = Var a
              | Lit Lit
              | Call a [a]
              | Alloca t
              | Load a [a]
              | Store a [a] a
              | Cast t a
              | Undef t
              | BinOp Op a a
              | If a (Block t a) (Block t a)
  deriving (Eq, Show, Read, Generic, PrettyVal, Functor, Foldable)

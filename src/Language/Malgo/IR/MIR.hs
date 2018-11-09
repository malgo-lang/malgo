module Language.Malgo.IR.MIR where

import           Data.Outputable
import           Language.Malgo.Pretty
import           Universum

-- K正規化とクロージャ変換済みの中間表現
-- クロージャはタプルで表現する
data Expr a = Var a
            | Literal Literal
            | BinOp Op a a
            | If a (Block a) (Block a)
            | Apply a a
            | Tuple [a]
            | TupleAccess a Int
            | Cast a (MType a)

data Stmt a = Let a (Expr a)
            | Ret (Expr a)

newtype Block a = Block [Stmt a]

data Decl a = Decl a [a] (Block a)

data Op
data Literal
data MType a

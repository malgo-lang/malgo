module Language.Malgo.IR where

import           Language.Malgo.Syntax (Name, Type)
import qualified Language.Malgo.Syntax as S

data Inst = Add Name Value Value
          | Sub Name Value Value
          | Mul Name Value Value
          | Div Name Value Value
          | Cmp Op Name Value Value
          | Br Value [Inst] [Inst]
  deriving (Eq, Show)

data Op = Eq | Lt | Gt | Le | Ge
  deriving (Eq, Show)

data Value = Reg Name
           | Int Integer
  deriving (Eq, Show)

{--

Add (Int 1) (Int 2)
==> Add [temp1] (Int 1) (Int 2)

Lt (Var (Sym "x")) (Var (Id 1))
==> Cmp Lt [temp1] (Reg "x") (Reg [offset+1])

If (Lt (Int 1) (Int 2)) [then] [else]
==> Cmp Lt [temp1] (Int 1) (Int 2)
    Br [temp1] [trans [then]] [trans [else]]

--}

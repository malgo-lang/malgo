module Language.Malgo.TigerSyn where

import qualified Language.Malgo.SExpr as S
import Language.Malgo.SExpr (Name)

data Var = SimpleVar Name
         | FieldVar Var Name
         | SubscriptVar Var Exp

data Exp = VarExp Var
         | NilExp
         | IntExp Integer
         | StringExp String
         | CallExp Name [Exp]
         | OpExp Exp Oper Exp
         | RecordExp [(Name, Exp)] Name
         | SeqExp [Exp]
         | AssignExp Var Exp
         | IfExp Exp Exp (Maybe Exp)
         | WhileExp Exp Exp
         | ForExp Name

data Oper

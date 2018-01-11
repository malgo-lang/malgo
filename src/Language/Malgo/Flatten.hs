module Language.Malgo.Flatten (flatten)where

import           Debug.Trace
import           Language.Malgo.HIR

flatten :: Show a => Expr a -> Expr a
flatten (If c t f) =
  If c (flatten t) (flatten f)
flatten (Let (ValDec x e1) e2) =
  insert (flatten e1)
  where insert (Let dec e3) =
          Let dec (insert e3)
        insert e =
          Let (ValDec x e) (flatten e2)
flatten (Let (FunDec fn params e1) e2) =
  Let (FunDec fn params (flatten e1)) (flatten e2)
flatten (Let (ExDec name orig) e) = Let (ExDec name orig) (flatten e)
flatten e = e

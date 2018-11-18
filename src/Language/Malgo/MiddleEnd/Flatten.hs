module Language.Malgo.MiddleEnd.Flatten where

import           Language.Malgo.IR.AST

flatten :: Expr a -> Expr a
flatten (BinOp ss op e1 e2) = BinOp ss op (flatten e1) (flatten e2)
flatten (If ss c t f) = If ss (flatten c) (flatten t) (flatten f)
flatten (Let ss0 (NonRec ss1 x mts0 e0) e1) = go (flatten e0)
  where
    go (Let ss2 bind e3) = Let ss2 bind (go e3)
    go e                 = Let ss0 (NonRec ss1 x mts0 e) (flatten e1)
flatten (Let ss0 (Rec ss1 f xs mts0 e0) e1) =
  Let ss0 (Rec ss1 f xs mts0 (flatten e0)) e1
flatten (Let ss0 (TuplePat ss1 xs mts0 e0) e1) = go (flatten e0)
  where
    go (Let ss2 bind e3) = Let ss2 bind (go e3)
    go e                 = Let ss0 (TuplePat ss1 xs mts0 e) (flatten e1)
flatten (Apply ss e1 e2) = Apply ss (flatten e1) (flatten e2)
flatten (Tuple ss xs) = Tuple ss (map flatten xs)
flatten e = e

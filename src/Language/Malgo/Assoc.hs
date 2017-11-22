module Language.Malgo.Assoc where

import           Language.Malgo.KNormal

assoc :: Expr -> Expr
assoc (If c t f, ty) =
  (If c (assoc t) (assoc f), ty)
assoc (Let (ValDec x xt e1) e2, ty) =
  insert (assoc e1)
  where insert (Let (ValDec y yt e3) e4, ty') =
          (Let (ValDec y yt e3) (insert e4), ty')
        insert (Let (fundef@FunDec{}) body, ty') =
          (Let fundef (insert body), ty')
        insert e = (Let (ValDec x xt e) (assoc e2), ty)
assoc (Let (FunDec fn params retTy e1) e2, ty) =
  (Let (FunDec fn params retTy (assoc e1)) (assoc e2), ty)
assoc e = e

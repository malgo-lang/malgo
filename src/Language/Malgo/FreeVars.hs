module Language.Malgo.FreeVars where

import           Data.List
import           Language.Malgo.HIR

-- Externはすべて自由変数とする
-- 実際に利用するときは、[Extern a] -> [a]が別に必要

fvExpr :: Eq a => Expr a -> [a]
fvExpr (Var x)        = [x]
fvExpr (Int _)        = []
fvExpr (Float _)      = []
fvExpr (Bool _)       = []
fvExpr (Char _)       = []
fvExpr (String _)     = []
fvExpr Unit           = []
fvExpr (Call fn args) = if fn `elem` args
                        then args
                        else fn : args
fvExpr (Let decl e) =
  fvExpr e \\ knowns decl
fvExpr (If c t f) =
  delete c $ union (fvExpr t) (fvExpr f)
fvExpr (BinOp _ x y) =
  if x == y
  then [x]
  else [x, y]

knowns :: Decl a -> [a]
knowns (ValDec x _)         = [x]
knowns (FunDec fn params _) = fn : params

fvDecl :: Eq a => Decl a -> [a]
fvDecl (ValDec x e) =
  delete x $ fvExpr e
fvDecl (FunDec fn params e) =
  fvExpr e \\ (fn : params)

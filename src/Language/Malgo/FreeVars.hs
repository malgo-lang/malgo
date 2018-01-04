module Language.Malgo.FreeVars where

import           Data.List
import qualified Language.Malgo.HIR       as H
import qualified Language.Malgo.MIR       as M
import           Language.Malgo.TypeCheck (TypedID (..))

class FreeVars f where
  freevars :: f TypedID -> [TypedID]

instance FreeVars H.Expr where
  freevars (H.Var x)        = [x]
  freevars (H.Int _)        = []
  freevars (H.Float _)      = []
  freevars (H.Bool _)       = []
  freevars (H.Char _)       = []
  freevars (H.String _)     = []
  freevars H.Unit           = []
  freevars (H.Call fn args) = if fn `elem` args
                              then args
                              else fn : args
  freevars (H.Let (H.ValDec x e1) e2) =
    freevars e1 `union` delete x (freevars e2)
  freevars (H.Let (H.FunDec fn params e1) e2) =
    let zs = freevars e1 \\ params
    in delete fn $ zs `union` freevars e2
  freevars (H.If c t f) =
    nub $ delete c $ union (freevars t) (freevars f)
  freevars (H.BinOp _ x y) =
    if x == y
    then [x]
    else [x, y]

instance FreeVars M.Expr where
  freevars (M.Var x)            = [x]
  freevars (M.Int _)            = []
  freevars (M.Float _)          = []
  freevars (M.Bool _)           = []
  freevars (M.Char _)           = []
  freevars (M.String _)         = []
  freevars M.Unit               = []
  freevars (M.CallDir fn args)  = nub $ fn : args
  freevars (M.CallCls cls args) = nub $ cls : args
  freevars (M.Let (M.ValDec x e1) e2) =
    freevars e1 `union` delete x (freevars e2)
  freevars (M.Let (M.ClsDec x _ fv) e) =
    delete x (fv `union` freevars e)
  freevars (M.Let (M.ExDec x _) e) =
    delete x (freevars e)
  freevars (M.If c t f) =
    nub $ delete c $ union (freevars t) (freevars f)
  freevars (M.BinOp _ x y) =
    if x == y
    then [x]
    else [x, y]

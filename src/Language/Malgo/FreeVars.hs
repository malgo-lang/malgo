module Language.Malgo.FreeVars where

import           Data.List
import qualified Language.Malgo.HIR       as H
import qualified Language.Malgo.MIR       as M
import           Language.Malgo.TypeCheck (TypedID (..))

class FreeVars f where
  knowns :: f TypedID -> [TypedID]
  freevars :: f TypedID -> [TypedID]

instance FreeVars H.Expr where
  knowns _ = undefined
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
  freevars (H.Let decl e) =
    freevars e \\ knowns decl
  freevars (H.If c t f) =
    nub $ delete c $ union (freevars t) (freevars f)
  freevars (H.BinOp _ x y) =
    if x == y
    then [x]
    else [x, y]

instance FreeVars M.Expr where
  knowns _ = []
  freevars (M.Var x)            = [x]
  freevars (M.Int _)            = []
  freevars (M.Float _)          = []
  freevars (M.Bool _)           = []
  freevars (M.Char _)           = []
  freevars M.Unit               = []
  freevars (M.MakeCls fn fv)    = nub $ fn : fv
  freevars (M.CallDir fn args)  = nub $ fn : args
  freevars (M.CallCls cls args) = nub $ cls : args
  freevars (M.If c t f) =
    nub $ delete c $ union (freevars t) (freevars f)
  freevars (M.BinOp _ x y) =
    if x == y
    then [x]
    else [x, y]

instance FreeVars H.Decl where
  freevars (H.ValDec x e) =
    delete x $ freevars e
  freevars (H.FunDec fn params e) =
    freevars e \\ (fn : params)
  knowns (H.ValDec x _)         = [x]
  knowns (H.FunDec fn params _) = fn : params

instance FreeVars M.Instr where
  knowns _ = []
  freevars (x M.:= e) = delete x $ freevars e
  freevars (M.Do x)   = freevars x

instance FreeVars M.Decl where
  knowns (M.FunDec fn params capture _) =
    nub $ fn : params ++ capture
  knowns (M.StrDec x _) = [x]
  knowns (M.ExDec x _) = [x]
  freevars d@(M.FunDec _ _ _ e) =
    (nub . concat $ map freevars e) \\ knowns d
  freevars (M.StrDec _ _) = []
  freevars (M.ExDec _ _) = []

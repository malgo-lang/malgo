{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.FreeVars (FreeVars(..))where

import           Data.List              (delete, nub, union, (\\))
import qualified Language.Malgo.HIR     as H
import qualified Language.Malgo.MIR     as M
import           Language.Malgo.Prelude
import           Language.Malgo.TypedID

class FreeVars f where
  freevars :: f TypedID -> [TypedID]

instance FreeVars H.Expr where
  freevars (H.Var x) = [x]
  freevars (H.Int _) = []
  freevars (H.Float _) = []
  freevars (H.Bool _) = []
  freevars (H.Char _) = []
  freevars (H.String _) = []
  freevars H.Unit = []
  freevars (H.Tuple xs) = xs
  freevars (H.TupleAccess x _) = [x]
  freevars (H.Call _ args) = args
  freevars (H.Let (H.ValDec x e1) e2) =
    freevars e1 `union` delete x (freevars e2)
  freevars (H.Let (H.ExDec name _) e) = delete name $ freevars e
  freevars (H.Let (H.FunDecs fs) e) =
    let zs = nub $ sort $ concatMap freevars fs
    in (zs `union` freevars e) \\ fns
    where fns = map (\(H.FunDec fn _ _) -> fn) fs
    -- let zs = freevars e1 \\ params
    -- in delete fn $ zs `union` freevars e2
  freevars (H.If c t f) = nub $ delete c $ union (freevars t) (freevars f)
  freevars (H.BinOp _ x y) =
    if x == y
      then [x]
      else [x, y]

instance FreeVars H.FunDec where
  freevars (H.FunDec fn params e1) = freevars e1 \\ (fn : params)

instance FreeVars M.Expr where
  freevars (M.Var x) = [x]
  freevars (M.Int _) = []
  freevars (M.Float _) = []
  freevars (M.Bool _) = []
  freevars (M.Char _) = []
  freevars (M.String _) = []
  freevars M.Unit = []
  freevars (M.Tuple xs) = xs
  freevars (M.TupleAccess x _) = [x]
  freevars (M.CallDir _ args) = nub args
  freevars (M.CallCls cls args) = nub $ cls : args
  freevars (M.Let (M.ValDec x e1) e2) =
    freevars e1 `union` delete x (freevars e2)
  freevars (M.Let (M.ClsDec x _ fv) e) = delete x (fv `union` freevars e)
  freevars (M.If c t f) = nub $ delete c $ union (freevars t) (freevars f)
  freevars (M.BinOp _ x y) =
    if x == y
      then [x]
      else [x, y]

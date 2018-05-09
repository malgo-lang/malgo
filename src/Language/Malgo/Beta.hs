{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Beta
  ( betaTrans
  ) where

import Control.Lens (at, set, view)
import Language.Malgo.Prelude
import Language.Malgo.Syntax
import Language.Malgo.TypedID

betaTrans :: Expr TypedID -> Expr TypedID
betaTrans expr = opt mempty expr

isConst :: Expr TypedID -> Bool
isConst Var {} = True
isConst Int {} = True
isConst Float {} = True
isConst Bool {} = True
isConst Char {} = True
isConst String {} = True
isConst Unit {} = True
isConst (Tuple _ xs) = all isConst xs
isConst (TupleAccess _ t@Tuple {} _) = isConst t
isConst TupleAccess {} = False
isConst Call {} = False
isConst Fn {} = False
isConst (Seq _ e1 e2) = isConst e1 && isConst e2
isConst Let {} = False
isConst (If _ c t f) = isConst c && isConst t && isConst f
isConst BinOp {} = False

opt :: Map TypedID (Expr TypedID) -> Expr TypedID -> Expr TypedID
opt env v@(Var _ x) = fromMaybe v (view (at x) env)
opt env e@(TupleAccess _ (Tuple _ xs) idx) =
  if isConst e
    then maybe e (opt env) (atMay xs idx)
    else e
opt env (Call i fn args) = Call i (opt env fn) (map (opt env) args)
opt env (Fn i params body) = Fn i params (opt env body)
opt env (Let i decs e) = Let i decs' $ opt env' e
  where
    (decs', env') = optDecl env decs []
opt env (If i c t f) =
  case c' of
    (Bool _ True) -> t
    (Bool _ False) -> f
    _ -> If i c' (opt env t) (opt env f)
  where
    c' = opt env c
opt env (BinOp i op x y) =
  case (op, opt env x, opt env y) of
    (Add, Int _ a, Int _ b) -> Int i $ a + b
    (Sub, Int _ a, Int _ b) -> Int i $ a - b
    (Mul, Int _ a, Int _ b) -> Int i $ a * b
    (FAdd, Float _ a, Float _ b) -> Float i $ a + b
    (FSub, Float _ a, Float _ b) -> Float i $ a - b
    (FMul, Float _ a, Float _ b) -> Float i $ a * b
    (_, x', y') -> BinOp i op x' y'
opt _ e = e

optDecl :: Map TypedID (Expr TypedID) -> [Decl TypedID] -> [Decl TypedID] -> ([Decl TypedID], Map TypedID (Expr TypedID))
optDecl env [] acc = (reverse acc, env)
optDecl env (ValDec i name ty val:xs) acc =
  if isConst val'
    then optDecl (set (at name) (pure val') env) xs (ValDec i name ty val' : acc)
    else optDecl env xs (ValDec i name ty val' : acc)
  where
    val' = opt env val
optDecl env (FunDec i name params retty body:xs) acc = optDecl env xs (FunDec i name params retty (opt env body) : acc)
optDecl env (dec:xs) acc = optDecl env xs (dec : acc)

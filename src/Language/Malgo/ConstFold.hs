{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.ConstFold (conv) where

import Language.Malgo.Syntax
import Language.Malgo.Prelude
import Language.Malgo.TypeCheck

conv :: Expr TypedID -> Expr TypedID
conv expr = opt mempty expr

isConst :: Expr TypedID -> Bool
isConst Var{} = True
isConst Int{} = True
isConst Float{} = True
isConst Bool{} = True
isConst Char{} = True
isConst String{} = True
isConst Unit{} = True
isConst (Tuple _ xs) = all isConst xs
isConst (TupleAccess _ t@Tuple{} _) = isConst t
isConst TupleAccess{} = False
isConst Call{} = False
isConst Fn{} = False
isConst (Seq _ e1 e2) = isConst e1 && isConst e2
isConst Let{} = False
isConst (If _ c t f) = isConst c && isConst t && isConst f
isConst BinOp{} = False

opt :: Map TypedID (Expr TypedID) -> Expr TypedID -> Expr TypedID
opt env v@(Var _ x) = fromMaybe v (lookup x env)
opt env e@(TupleAccess _ (Tuple _ xs) idx) =
  if isConst e
  then maybe e (opt env) (atMay xs idx)
  else e
opt env (Fn info params body) =
  Fn info params (opt env body)
opt env (Let info decs e) =
  Let info decs' $ opt env' e
  where (decs', env') = optDecl env decs []
opt env (If info c t f) =
  case c' of
    (Bool _ True) -> t
    (Bool _ False) -> f
    _ -> If info c' (opt env t) (opt env f)
  where c' = opt env c
opt env (BinOp info op x y) =
  BinOp info op (opt env x) (opt env y)
opt _ e = e

optDecl ::
  Map TypedID (Expr TypedID)
  -> [Decl TypedID]
  -> [Decl TypedID]
  -> ([Decl TypedID], Map TypedID (Expr TypedID))
optDecl env [] acc =
  (reverse acc, env)
optDecl env (ValDec info name ty val:xs) acc =
  if isConst val'
  then optDecl (insert name val' env) xs (ValDec info name ty val':acc)
  else optDecl env xs (ValDec info name ty val':acc)
  where val' = opt env val
optDecl env (FunDec info name params retty body:xs) acc =
  optDecl env xs (FunDec info name params retty (opt env body):acc)
optDecl env (dec:xs) acc =
  optDecl env xs (dec:acc)

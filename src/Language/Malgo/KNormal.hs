{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Malgo.KNormal where

import           Control.Monad.Except
import           Language.Malgo.HIR
import           Language.Malgo.Rename    (ID (..))
import qualified Language.Malgo.Syntax    as S
import           Language.Malgo.Type
import           Language.Malgo.TypeCheck
import           Language.Malgo.Utils
import           Text.PrettyPrint

data KEnv = KEnv
  deriving Show

instance Env KEnv where
  initEnv = KEnv

type KNormal m a = MalgoT KEnv m a

knormal :: Monad m => S.Expr TypedID -> KNormal m (Expr TypedID)
knormal e =
  transExpr (flattenLet e)

throw :: Monad m => Info -> Doc -> KNormal m a
throw info mes = throwError (KNormalError info mes)

flattenLet :: S.Expr TypedID -> S.Expr TypedID
flattenLet (S.Let info [decl] body) =
  S.Let info [decl] body
flattenLet (S.Let info (d:ds) body) =
  S.Let info [d] (flattenLet (S.Let info ds body))
flattenLet e = e

newTmp :: Monad m => Type -> KNormal m TypedID
newTmp typ = do
  c <- newUniq
  return (TypedID (Internal "$k" c) typ)

newUnused :: Monad m => KNormal m TypedID
newUnused = do
  c <- newUniq
  return (TypedID (Internal "$_" c) "Unit")

transOp :: Monad m => S.Op -> Type -> KNormal m Op
transOp S.Add _  = return Add
transOp S.Sub _  = return Sub
transOp S.Mul _  = return Mul
transOp S.Div _  = return Div
transOp S.FAdd _ = return FAdd
transOp S.FSub _ = return FSub
transOp S.FMul _ = return FMul
transOp S.FDiv _ = return FDiv
transOp S.Mod _  = return Mod
transOp S.Eq ty  = return $ Eq ty
transOp S.Neq ty = return $ Neq ty
transOp S.Lt ty  = return $ Lt ty
transOp S.Gt ty  = return $ Gt ty
transOp S.Le ty  = return $ Le ty
transOp S.Ge ty  = return $ Ge ty
transOp S.And _  = return And
transOp S.Or _   = return Or

insertLet :: Monad m => S.Expr TypedID -> (TypedID -> KNormal m (Expr TypedID)) -> KNormal m (Expr TypedID)
insertLet (S.Var _ x) k = k x
insertLet v k = do
  x <- newTmp (typeOf v)
  v' <- transExpr v
  e <- k x
  return (Let (ValDec x v') e)

bind :: Monad m
     => [S.Expr TypedID]
     -> [TypedID]
     -> ([TypedID] -> KNormal m (Expr TypedID))
     -> KNormal m (Expr TypedID)
bind [] args k     = k (reverse args)
bind (x:xs) args k = insertLet x (\x' -> bind xs (x':args) k)

transExpr :: Monad m => S.Expr TypedID -> KNormal m (Expr TypedID)
transExpr (S.Var _ x)    = return (Var x)
transExpr (S.Int _ x)    = return (Int x)
transExpr (S.Float _ x)  = return (Float x)
transExpr (S.Bool _ x)   = return (Bool x)
transExpr (S.Char _ x)   = return (Char x)
transExpr (S.String _ x) = return (String x)
transExpr (S.Unit _)     = return Unit
transExpr (S.Call _ fn args) =
  insertLet fn (\fn' ->
                  bind args [] (return . Call fn'))
transExpr (S.BinOp _ op e1 e2) = do
  op' <- transOp op (typeOf e1)
  insertLet e1 (\x -> insertLet e2 (return . BinOp op' x))
transExpr (S.If _ c t f) =
  insertLet c (\c' -> do
                  t' <- transExpr t
                  f' <- transExpr f
                  return (If c' t' f'))
transExpr (S.Let _ [S.ValDec _ name _ val] body) = do
  val' <- transExpr val
  body' <- transExpr body
  return (Let (ValDec name val') body')
transExpr (S.Let _ [S.FunDec _ fn params _ fbody] body) = do
  fbody' <- transExpr fbody
  body' <- transExpr body
  return (Let (FunDec fn (map fst params) fbody') body')
transExpr (S.Let _ [S.ExDec _ name _ orig] body) = do
  body' <- transExpr body
  return (Let (ExDec name orig) body')
transExpr (S.Let _ _ e) = transExpr e
transExpr (S.Seq _ e1 e2) = do
  unused <- newUnused
  e1' <- transExpr e1
  e2' <- transExpr e2
  return $ Let (ValDec unused e1') e2'

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Malgo.KNormal
  ( knormal
  ) where

import           Language.Malgo.HIR
import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import qualified Language.Malgo.Syntax  as S
import           Language.Malgo.Type
import           Language.Malgo.TypedID

knormal :: MonadMalgo UniqSupply m => S.Expr TypedID -> m (Expr TypedID)
knormal e = transExpr e

newTmp :: MonadMalgo UniqSupply m => Name -> Type -> m TypedID
newTmp name typ = TypedID <$> (ID ("$" <> name) <$> newUniq) <*> return typ

newUnused :: MonadMalgo UniqSupply m => m TypedID
newUnused = newTmp "_" "Unit"

transOp :: MonadMalgo UniqSupply m => S.Op -> Type -> m Op
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

insertLet :: MonadMalgo UniqSupply m => S.Expr TypedID -> (TypedID -> m (Expr TypedID)) -> m (Expr TypedID)
insertLet (S.Var _ x) k = k x
insertLet v k = do
  x <- newTmp "k" (typeOf v)
  v' <- transExpr v
  e <- k x
  return (Let (ValDec x v') e)

bind :: MonadMalgo UniqSupply m => [S.Expr TypedID] -> [TypedID] -> ([TypedID] -> m (Expr TypedID)) -> m (Expr TypedID)
bind [] args k     = k (reverse args)
bind (x:xs) args k = insertLet x (\x' -> bind xs (x' : args) k)

transExpr :: MonadMalgo UniqSupply m => S.Expr TypedID -> m (Expr TypedID)
transExpr (S.Var _ x) = return (Var x)
transExpr (S.Int _ x) = return (Int x)
transExpr (S.Float _ x) = return (Float x)
transExpr (S.Bool _ x) = return (Bool x)
transExpr (S.Char _ x) = return (Char x)
transExpr (S.String _ x) = return (String x)
transExpr (S.Unit _) = return Unit
transExpr (S.Tuple _ xs) = bind xs [] (return . Tuple)
transExpr (S.TupleAccess _ e i) = insertLet e (\e' -> return $ TupleAccess e' i)
transExpr (S.Fn _ params body) = do
  body' <- transExpr body
  fn <- newFnId
  return (Let (FunDecs [FunDec fn (map fst params) body']) (Var fn))
  where
    newFnId = newTmp "lambda" (FunTy (map snd params) (typeOf body))
transExpr (S.Call _ fn args) = insertLet fn (\fn' -> bind args [] (return . Call fn'))
transExpr (S.BinOp _ op e1 e2) = do
  op' <- transOp op (typeOf e1)
  insertLet e1 (\x -> insertLet e2 (return . BinOp op' x))
transExpr (S.If _ c t f) =
  insertLet
    c
    (\c' -> do
       t' <- transExpr t
       f' <- transExpr f
       return (If c' t' f'))
transExpr (S.Let info (S.ValDec _ name _ val:ds) body) = do
  val' <- transExpr val
  rest <- transExpr (S.Let info ds body)
  return (Let (ValDec name val') rest)
transExpr (S.Let info (S.FunDec _ fn params _ fbody:ds) body) = do
  fbody' <- transExpr fbody
  rest <- transExpr (S.Let info ds body)
  return (Let (FunDecs [FunDec fn (map fst params) fbody']) rest)
transExpr (S.Let info (S.ExDec _ name _ orig:ds) body) = Let (ExDec name orig) <$> transExpr (S.Let info ds body)
transExpr (S.Let _ [] body) = transExpr body
transExpr (S.Seq _ e1 e2) = do
  unused <- newUnused
  e1' <- transExpr e1
  e2' <- transExpr e2
  return $ Let (ValDec unused e1') e2'

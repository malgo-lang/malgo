{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Malgo.KNormal (knormal) where

import           Language.Malgo.HIR
import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Prelude
import qualified Language.Malgo.Syntax  as S
import           Language.Malgo.Type
import           Language.Malgo.TypedID

type KNormal a = Malgo Int a

knormal :: S.Expr TypedID -> KNormal (Expr TypedID)
knormal e = transExpr e

newTmp :: Type -> KNormal TypedID
newTmp typ = do
  c <- newUniq
  pure (TypedID (ID "$k" c) typ)

newUnused :: KNormal TypedID
newUnused = do
  c <- newUniq
  pure (TypedID (ID "$_" c) "Unit")

transOp :: S.Op -> Type -> KNormal Op
transOp S.Add _  = pure Add
transOp S.Sub _  = pure Sub
transOp S.Mul _  = pure Mul
transOp S.Div _  = pure Div
transOp S.FAdd _ = pure FAdd
transOp S.FSub _ = pure FSub
transOp S.FMul _ = pure FMul
transOp S.FDiv _ = pure FDiv
transOp S.Mod _  = pure Mod
transOp S.Eq ty  = pure $ Eq ty
transOp S.Neq ty = pure $ Neq ty
transOp S.Lt ty  = pure $ Lt ty
transOp S.Gt ty  = pure $ Gt ty
transOp S.Le ty  = pure $ Le ty
transOp S.Ge ty  = pure $ Ge ty
transOp S.And _  = pure And
transOp S.Or _   = pure Or

insertLet ::
  S.Expr TypedID
  -> (TypedID -> KNormal (Expr TypedID))
  -> KNormal (Expr TypedID)
insertLet (S.Var _ x) k = k x
insertLet v k = do
  x <- newTmp (typeOf v)
  v' <- transExpr v
  e <- k x
  pure (Let (ValDec x v') e)

bind ::
  [S.Expr TypedID]
  -> [TypedID]
  -> ([TypedID] -> KNormal (Expr TypedID))
  -> KNormal (Expr TypedID)
bind [] args k     = k (reverse args)
bind (x:xs) args k = insertLet x (\x' -> bind xs (x' : args) k)

transExpr :: S.Expr TypedID -> KNormal (Expr TypedID)
transExpr (S.Var _ x) = pure (Var x)
transExpr (S.Int _ x) = pure (Int x)
transExpr (S.Float _ x) = pure (Float x)
transExpr (S.Bool _ x) = pure (Bool x)
transExpr (S.Char _ x) = pure (Char x)
transExpr (S.String _ x) = pure (String x)
transExpr (S.Unit _) = pure Unit
transExpr (S.Tuple _ xs) = bind xs [] (pure . Tuple)
transExpr (S.TupleAccess _ e i) = insertLet e (\e' -> pure $ TupleAccess e' i)
transExpr (S.Fn _ params body) = do
  body' <- transExpr body
  fn <- newFnId
  pure (Let (FunDecs [FunDec fn (map fst params) body']) (Var fn))
  where newFnId = do
          c <- newUniq
          pure $ TypedID (ID "lambda" c) (FunTy (map snd params) (typeOf body))
transExpr (S.Call _ fn args) =
  insertLet fn (\fn' -> bind args [] (pure . Call fn'))
transExpr (S.BinOp _ op e1 e2) = do
  op' <- transOp op (typeOf e1)
  insertLet e1 (\x -> insertLet e2 (pure . BinOp op' x))
transExpr (S.If _ c t f) =
  insertLet
    c
    (\c' -> do
       t' <- transExpr t
       f' <- transExpr f
       pure (If c' t' f'))
transExpr (S.Let info (S.ValDec _ name _ val:ds) body) = do
  val' <- transExpr val
  rest <- transExpr (S.Let info ds body)
  pure (Let (ValDec name val') rest)
transExpr (S.Let info (S.FunDec _ fn params _ fbody:ds) body) = do
  fbody' <- transExpr fbody
  rest <- transExpr (S.Let info ds body)
  pure (Let (FunDecs [FunDec fn (map fst params) fbody']) rest)
transExpr (S.Let info (S.ExDec _ name _ orig:ds) body) =
  Let (ExDec name orig) <$> transExpr (S.Let info ds body)
transExpr (S.Let _ [] body) = transExpr body
transExpr (S.Seq _ e1 e2) = do
  unused <- newUnused
  e1' <- transExpr e1
  e2' <- transExpr e2
  pure $ Let (ValDec unused e1') e2'

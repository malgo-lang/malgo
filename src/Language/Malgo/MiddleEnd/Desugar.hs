{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.Desugar (Desugar) where

import           Language.Malgo.Id
import           Language.Malgo.IR.Core
import qualified Language.Malgo.IR.Syntax    as S
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.TypeRep.Type

data Desugar

instance Pass Desugar (S.Expr (Id Type)) (Exp (Id Type)) where
  passName = "Desugar"
  isDump = const False -- TODO: dumpDesugar
  trans e = toExp e

newTmp :: MonadUniq m => Type -> m (Id Type)
newTmp t = newId t "$d"

toExp :: MonadUniq f => S.Expr (Id Type) -> f (Exp (Id Type))
toExp (S.Var _ x) = pure $ Atom $ Var x
toExp (S.Int _ x) = do
  v <- newTmp intTy
  pure $ Let [(v, Pack (Con "Int" 1) [Unboxed (Int x)])] $ Atom $ Var v
toExp (S.Float _ x) = do
  v <- newTmp floatTy
  pure $ Let [(v, Pack (Con "Float" 1) [Unboxed (Float x)])] $ Atom $ Var v
toExp (S.Bool _ x) = do
  v <- newTmp boolTy
  let o = if x then Pack (Con "True" 0) [] else Pack (Con "False" 0) []
  pure $ Let [(v, o)] $ Atom $ Var v
toExp (S.Char _ x) = do
  v <- newTmp charTy
  pure $ Let [(v, Pack (Con "Char" 1) [Unboxed $ Char x])] $ Atom $ Var v
toExp (S.String _ x) = do
  v <- newTmp stringTy
  pure $ Let [(v, Pack (Con "String" 1) [Unboxed $ String x])] $ Atom $ Var v

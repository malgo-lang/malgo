{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.TransToHIR
  ( TransToHIR
  )
where

import           Language.Malgo.ID
import           Language.Malgo.IR.HIR
import qualified Language.Malgo.IR.Syntax      as S
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude

data TransToHIR

instance Pass TransToHIR (S.Expr (ID Type)) (Expr (ID Type)) where
  passName = "TransToHIR"
  isDump   = dumpKNormal
  trans e = flattenExpr <$> transToHIR e

newTmp :: MonadMalgo f => String -> a -> f (ID a)
newTmp n t = newID t ("$" <> n)

insertLet :: MonadMalgo m => S.Expr (ID Type) -> WriterT (Endo (Expr (ID Type))) m (ID Type)
insertLet (S.Var _ x) = pure x
insertLet v           = do
  v' <- transToHIR v
  x  <- newTmp "k" (typeOf v')
  tell $ Endo $ Let x v'
  return x

appInsert :: Functor f => WriterT (Endo b) f b -> f b
appInsert m = uncurry (flip appEndo) <$> runWriterT m

transToHIR :: MonadMalgo m => S.Expr (ID Type) -> m (Expr (ID Type))
transToHIR (S.Var    _ a             ) = pure $ Var a
transToHIR (S.Int    _ x             ) = pure $ Lit $ Int x
transToHIR (S.Float  _ x             ) = pure $ Lit $ Float x
transToHIR (S.Bool   _ x             ) = pure $ Lit $ Bool x
transToHIR (S.Char   _ x             ) = pure $ Lit $ Char x
transToHIR (S.String _ x             ) = pure $ Lit $ String x
transToHIR (S.Unit _                 ) = pure $ Tuple []
transToHIR (S.Tuple _ xs             ) = appInsert $ Tuple <$> mapM insertLet xs
transToHIR (S.TupleAccess _ e    i   ) = appInsert $ TupleAccess <$> insertLet e <*> pure i
transToHIR (S.MakeArray   _ init size) = appInsert $ MakeArray <$> insertLet init <*> insertLet size
transToHIR (S.ArrayRead   _ arr  ix  ) = appInsert $ ArrayRead <$> insertLet arr <*> insertLet ix
transToHIR (S.ArrayWrite _ arr ix val) =
  appInsert $ ArrayWrite <$> insertLet arr <*> insertLet ix <*> insertLet val
transToHIR (  S.Call _ f  xs) = appInsert $ Call <$> insertLet f <*> mapM insertLet xs
transToHIR f@(S.Fn   _ ps e ) = do
  fn <- newTmp "lambda" (typeOf f)
  e' <- transToHIR e
  return $ LetRec [Def { name = fn, params = map fst ps, expr = e' }] (Var fn)
transToHIR (S.Seq _ e1                   e2  ) = appInsert $ insertLet e1 >> transToHIR e2
transToHIR (S.Let _ (S.ValDec _ n _ val) body) = Let n <$> transToHIR val <*> transToHIR body
transToHIR (S.Let _ (S.FunDec fundecs) body) =
  LetRec <$> mapM transFunDec fundecs <*> transToHIR body
 where
  transFunDec (_, fn, params, _, fbody) = do
    fbody' <- transToHIR fbody
    return Def { name = fn, params = map fst params, expr = fbody' }
transToHIR (S.Let _ (S.ExDec _ n _ orig) body) = case typeOf n of
  TyApp FunC (_ : ps) -> do
    params <- mapM (newTmp "x") ps
    LetRec [Def { name = n, params = params, expr = Prim orig (typeOf n) params }]
      <$> transToHIR body
  _ -> error "external variable is not supported"
transToHIR (S.If    _ c  t f) = appInsert $ If <$> insertLet c <*> transToHIR t <*> transToHIR f
transToHIR (S.BinOp _ op x y) = appInsert $ BinOp op' <$> insertLet x <*> insertLet y
 where
  op' = case op of
    S.Add  -> Add
    S.Sub  -> Sub
    S.Mul  -> Mul
    S.Div  -> Div
    S.Mod  -> Mod
    S.FAdd -> FAdd
    S.FSub -> FSub
    S.FMul -> FMul
    S.FDiv -> FDiv
    S.Eq   -> Eq
    S.Neq  -> Neq
    S.Lt   -> Lt
    S.Gt   -> Gt
    S.Le   -> Le
    S.Ge   -> Ge
    S.And  -> And
    S.Or   -> Or
transToHIR (S.Match _ s cs) =
  appInsert $ Match <$> insertLet s <*> mapM (\(p, e) -> crushPat p =<< transToHIR e) cs

crushPat :: MonadMalgo m => S.Pat (ID Type) -> Expr (ID Type) -> m (Pat (ID Type), Expr (ID Type))
crushPat (S.VarP   x ) e = pure (VarP x, e)
crushPat (S.TupleP xs) e = go xs [] e
 where
  go []              acc e = pure (TupleP $ reverse acc, e)
  go (S.VarP x : ps) acc e = go ps (x : acc) e
  go (p        : ps) acc e = do
    x <- newTmp "pat" $ typeOf p
    go ps (x : acc) =<< do
      clause <- crushPat p e
      pure $ Match x (clause :| [])

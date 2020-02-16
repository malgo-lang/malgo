{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.MiddleEnd.TransToHIR
  ( TransToHIR
  )
where

import           Language.Malgo.ID
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude

import           Language.Malgo.IR.HIR
import qualified Language.Malgo.IR.Syntax      as S

import           Language.Malgo.TypeRep.Type

data TransToHIR

instance Pass TransToHIR (S.Expr (ID Type)) (Expr (ID Type)) where
  passName = "TransToHIR"
  isDump   = dumpKNormal
  trans e = flattenExpr <$> transToHIR e

newTmp :: MonadUniq f => String -> a -> f (ID a)
newTmp n t = newID t ("$" <> n)

insertLet :: (MonadUniq f, MonadWriter (Endo (Expr (ID Type))) f) => S.Expr (ID Type) -> f (ID Type)
insertLet (S.Var _ x) = pure x
insertLet v           = do
  v' <- transToHIR v
  x  <- newTmp "k" (typeOf v')
  tell $ Endo $ Let x v'
  pure x

insertLet' :: (MonadUniq f, MonadWriter (Endo (Expr (ID Type))) f) => Expr (ID Type) -> f (ID Type)
insertLet' (Var x) = pure x
insertLet' v       = do
  x <- newTmp "k" (typeOf v)
  tell $ Endo $ Let x v
  pure x

appInsert :: Functor f => WriterT (Endo b) f b -> f b
appInsert m = uncurry (flip appEndo) <$> runWriterT m

transToHIR :: MonadUniq m => S.Expr (ID Type) -> m (Expr (ID Type))
transToHIR (S.Var    _ a        ) = pure $ Var a
transToHIR (S.Int    _ x        ) = pure $ Lit $ Int x
transToHIR (S.Float  _ x        ) = pure $ Lit $ Float x
transToHIR (S.Bool   _ x        ) = pure $ Lit $ Bool x
transToHIR (S.Char   _ x        ) = pure $ Lit $ Char x
transToHIR (S.String _ x        ) = pure $ Lit $ String x
transToHIR (S.Tuple  _ xs       ) = appInsert $ Tuple <$> mapM insertLet xs
transToHIR (S.Array  i (x :| xs)) = appInsert $ do
  arr <- insertLet $ S.MakeArray i x (S.Int i $ fromIntegral $ length (x : xs))
  forM_ (zip [1 ..] xs)
    $ \(i, v) -> insertLet' =<< ArrayWrite arr <$> insertLet' (Lit (Int i)) <*> insertLet v
  pure $ Var arr
transToHIR (S.MakeArray _ init size) = appInsert $ MakeArray <$> insertLet init <*> insertLet size
transToHIR (S.ArrayRead _ arr  ix  ) = appInsert $ ArrayRead <$> insertLet arr <*> insertLet ix
transToHIR (S.ArrayWrite _ arr ix val) =
  appInsert $ ArrayWrite <$> insertLet arr <*> insertLet ix <*> insertLet val
transToHIR (S.Call _ f  xs) = appInsert $ Call <$> insertLet f <*> mapM insertLet xs
transToHIR (S.Fn   _ ps e ) = do
  e' <- transToHIR e
  fn <- newTmp "lambda" (TyApp FunC (typeOf e' : map (typeOf . fst) ps))
  pure $ LetRec [Def { name = fn, params = map fst ps, expr = e' }] (Var fn)
transToHIR (S.Seq _ e1                   e2  ) = appInsert $ insertLet e1 >> transToHIR e2
transToHIR (S.Let _ (S.ValDec _ n _ val) body) = Let n <$> transToHIR val <*> transToHIR body
transToHIR (S.Let _ (S.FunDec fundecs) body) =
  LetRec <$> mapM transFunDec fundecs <*> transToHIR body
 where
  transFunDec (_, fn, params, _, fbody) = do
    fbody' <- transToHIR fbody
    pure Def { name = fn, params = map fst params, expr = fbody' }
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

crushPat :: MonadUniq m => S.Pat (ID Type) -> Expr (ID Type) -> m (Pat (ID Type), Expr (ID Type))
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

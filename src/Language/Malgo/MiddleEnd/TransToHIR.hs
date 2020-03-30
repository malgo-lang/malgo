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

import           Language.Malgo.Id
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude

import           Language.Malgo.IR.HIR
import qualified Language.Malgo.IR.Syntax      as S

import           Language.Malgo.TypeRep.Type

data TransToHIR

instance Pass TransToHIR (S.Expr (Id Type)) (Expr (Id Type)) where
  passName = "TransToHIR"
  isDump   = dumpKNormal
  trans e = flattenExpr <$> toExpr e

newTmp :: MonadUniq f => String -> a -> f (Id a)
newTmp n t = newId t ("$" <> n)

toVar :: (MonadUniq f, MonadWriter (Endo (Expr (Id Type))) f) => S.Expr (Id Type) -> f (Id Type)
toVar = def <=< toExpr

def :: (MonadUniq f, MonadWriter (Endo (Expr (Id Type))) f) => Expr (Id Type) -> f (Id Type)
def (Var x) = pure x
def v       = do
  x <- newTmp "k" (typeOf v)
  tell $ Endo $ Let x v
  pure x

runDef :: Functor f => WriterT (Endo b) f b -> f b
runDef m = uncurry (flip appEndo) <$> runWriterT m

toExpr :: MonadUniq m => S.Expr (Id Type) -> m (Expr (Id Type))
toExpr (S.Var    _ a        ) = pure $ Var a
toExpr (S.Int    _ x        ) = pure $ Lit $ Int x
toExpr (S.Float  _ x        ) = pure $ Lit $ Float x
toExpr (S.Bool   _ x        ) = pure $ Lit $ Bool x
toExpr (S.Char   _ x        ) = pure $ Lit $ Char x
toExpr (S.String _ x        ) = pure $ Lit $ String x
toExpr (S.Tuple  _ xs       ) = runDef $ Tuple <$> mapM toVar xs
toExpr (S.Array  _ (x :| xs)) = runDef $ do
  arr <- def =<< MakeArray <$> toVar x <*> def (Lit $ Int $ fromIntegral $ length (x : xs))
  ifor_ xs $ \i v -> def =<< ArrayWrite arr <$> def (Lit $ Int $ fromIntegral $ i + 1) <*> toVar v
  pure $ Var arr
toExpr (S.MakeArray _ init size  ) = runDef $ MakeArray <$> toVar init <*> toVar size
toExpr (S.ArrayRead _ arr  ix    ) = runDef $ ArrayRead <$> toVar arr <*> toVar ix
toExpr (S.ArrayWrite _ arr ix val) = runDef $ ArrayWrite <$> toVar arr <*> toVar ix <*> toVar val
toExpr (S.Call _ f  xs           ) = runDef $ Call <$> toVar f <*> mapM toVar xs
toExpr (S.Fn   _ ps e            ) = do
  e' <- toExpr e
  fn <- newTmp "lambda" (TyApp FunC (typeOf e' : map (typeOf . fst) ps))
  pure $ LetRec [Def { name = fn, params = map fst ps, expr = e' }] (Var fn)
toExpr (S.Seq _ e1                   e2  ) = runDef $ toVar e1 >> toExpr e2
toExpr (S.Let _ (S.ValDec _ n _ val) body) = Let n <$> toExpr val <*> toExpr body
toExpr (S.Let _ (S.FunDec fundecs) body) =
  LetRec <$> mapM (\(_, fn, ps, _, e) -> Def fn (map fst ps) <$> toExpr e) fundecs <*> toExpr body
toExpr (S.Let _ (S.ExDec _ n _ orig) body) = case typeOf n of
  TyApp FunC (_ : ps) -> do
    params <- mapM (newTmp "x") ps
    LetRec [Def { name = n, params = params, expr = Prim orig (typeOf n) params }] <$> toExpr body
  _ -> error "external variable is not supported"
toExpr (S.If    _ c  t f) = runDef $ If <$> toVar c <*> toExpr t <*> toExpr f
toExpr (S.BinOp _ op x y) = runDef $ BinOp op <$> toVar x <*> toVar y
toExpr (S.Match _ s cs) =
  runDef $ Match <$> toVar s <*> mapM (\(p, e) -> crushPat p =<< toExpr e) cs

crushPat :: MonadUniq m => S.Pat (Id Type) -> Expr (Id Type) -> m (Pat (Id Type), Expr (Id Type))
crushPat (S.VarP   x ) e = pure (VarP x, e)
crushPat (S.TupleP xs) e = go xs [] e
 where
  go []       acc e = pure (TupleP $ reverse acc, e)
  go (p : ps) acc e = do
    x      <- newTmp "pat" $ typeOf p
    clause <- crushPat p e
    go ps (x : acc) (Match x (clause :| []))

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Malgo.MiddleEnd.Desugar
  ( desugar,
  )
where

import Koriel.Core.Core
import Koriel.Core.Flat
import Koriel.Core.Type hiding (Type, typeOf)
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import qualified Language.Malgo.IR.Syntax as S
import Language.Malgo.TypeRep.Type hiding ((:->))

desugar :: (MonadUniq f, MonadFail f) => S.Expr (Id Type) -> f (Exp (Id C.Type))
desugar e = flat <$> evalStateT (toExp e) mempty

findVar :: (MonadFail m, MonadState (Map (Id Type) (Id C.Type)) m) => Id Type -> m (Id C.Type)
findVar v = do
  Just v <- gets (view (at v))
  pure v

toExp ::
  (MonadState (Map (Id Type) (Id C.Type)) m, MonadUniq m, MonadFail m) =>
  S.Expr (Id Type) ->
  m (Exp (Id C.Type))
toExp (S.Var _ x) = Atom . Var <$> findVar x
toExp (S.Int _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Int64 $ fromInteger x]
  where
    ty = SumT [con]
    con = Con "Int" [Int64T]
toExp (S.Float _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Double x]
  where
    ty = SumT [con]
    con = Con "Float" [DoubleT]
toExp (S.Bool _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Bool x]
  where
    ty = SumT [con]
    con = Con "Bool" [BoolT]
toExp (S.Char _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Char x]
  where
    ty = SumT [con]
    con = Con "Char" [CharT]
toExp (S.String _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ String x]
  where
    ty = SumT [con]
    con = Con "String" [StringT]
toExp (S.Tuple _ xs) = runDef $ do
  vs <- traverse (bind <=< toExp) xs
  let con = Con ("Tuple" <> length xs ^. toText) $ map C.typeOf vs
  let ty = SumT [con]
  runDef $ fmap Atom $ let_ ty $ Pack ty con vs
toExp (S.Array _ (x :| xs)) = runDef $ do
  x <- bind =<< toExp x
  arr <- let_ (ArrayT $ C.typeOf x) $ Array x $ Unboxed $ Int64 $ fromIntegral $ length xs + 1
  ifor_ xs $ \i v -> do
    v <- bind =<< toExp v
    bind $ ArrayWrite arr (Unboxed $ Int64 $ fromIntegral $ i + 1) v
  pure $ Atom arr
toExp (S.MakeArray _ a n) = runDef $ do
  a <- bind =<< toExp a
  n <- bind =<< toExp n
  [n] <- destruct (Atom n) $ Con "Int" [Int64T]
  Atom <$> let_ (ArrayT $ C.typeOf a) (Array a n)
toExp (S.ArrayRead _ a i) = runDef $ do
  a <- bind =<< toExp a
  i <- bind =<< toExp i
  [i] <- destruct (Atom i) $ Con "Int" [Int64T]
  Atom <$> bind (ArrayRead a i)
toExp (S.ArrayWrite _ a i x) = runDef $ do
  a <- bind =<< toExp a
  i <- bind =<< toExp i
  case C.typeOf a of
    ArrayT ty -> do
      x <- cast ty =<< toExp x
      [i] <- destruct (Atom i) $ Con "Int" [Int64T]
      Atom <$> bind (ArrayWrite a i x)
    _ -> bug Unreachable
toExp (S.Call _ f xs) = runDef $ do
  f <- bind =<< toExp f
  case C.typeOf f of
    ps :-> _ -> Call f <$> zipWithM (\x p -> cast p =<< toExp x) xs ps
    _ -> bug Unreachable
toExp (S.Fn _ ps e) = runDef $ do
  ps' <- traverse ((\p -> newId (p ^. idName) (C.typeOf p)) . fst) ps
  e <- do
    zipWithM_ (\(p, _) p' -> at p ?= p') ps ps'
    toExp e
  Atom <$> let_ (map C.typeOf ps' :-> C.typeOf e) (Fun ps' e)
toExp (S.Seq _ e1 e2) = runDef $ do
  _ <- bind =<< toExp e1
  toExp e2
toExp (S.Let _ (S.ValDec _ a _ v) e) = runDef $ do
  Var v <- cast (C.typeOf a) =<< toExp v
  at a ?= v
  toExp e
toExp (S.Let _ (S.ExDec _ prim _ primName) e) = case C.typeOf $ prim ^. idMeta of
  ta :-> tb -> runDef $ do
    ps <- traverse (newId "a") ta
    Var prim' <- let_ (ta :-> tb) $ Fun ps $ ExtCall primName (ta :-> tb) (map Var ps)
    at prim ?= prim'
    toExp e
  _ -> bug Unreachable
toExp (S.Let _ (S.FunDec fs) e) = do
  for_ fs $ \(_, f, _, _, _) ->
    (at f ?=) =<< newId (f ^. idName) (C.typeOf f)
  Let <$> traverse toFun fs <*> toExp e
  where
    toFun (_, f@(C.typeOf -> _ :-> r), ps, _, body) = do
      ps' <- traverse ((\p -> newId (p ^. idName) (C.typeOf p)) . fst) ps
      body <- runDef $ do
        zipWithM_ (\(p, _) p' -> at p ?= p') ps ps'
        Atom <$> (cast r =<< toExp body)
      f <- findVar f
      pure (f, Fun ps' body)
    toFun _ = bug Unreachable
toExp (S.If _ c t f) = runDef $ do
  c <- toExp c
  [c'] <- destruct c (Con "Bool" [BoolT])
  t <- Switch (Bool True) <$> toExp t
  f <- Switch (Bool False) <$> toExp f
  pure $ Match (Atom c') (t :| [f])
toExp e@(S.BinOp _ opr x y) = runDef $ do
  let SumT (toList -> [xCon]) = C.typeOf $ typeOf x
  let SumT (toList -> [yCon]) = C.typeOf $ typeOf y
  lexp <- toExp x
  rexp <- toExp y
  [lval] <- destruct lexp xCon
  [rval] <- destruct rexp yCon
  result <- bind $ BinOp opr lval rval
  let retType@(SumT (toList -> [rCon])) = C.typeOf $ typeOf e
  Atom <$> let_ retType (Pack retType rCon [result])
toExp (S.Match _ e cs) = do
  e <- toExp e
  cs <- traverse (\(p, v) -> crushPat p $ toExp v) cs
  pure $ Match e cs

crushPat ::
  (MonadUniq m, MonadState (Map (Id Type) (Id C.Type)) m) =>
  S.Pat (Id Type) ->
  m (Exp (Id C.Type)) ->
  m (Case (Id C.Type))
crushPat (S.VarP _ x) = \e -> do
  x' <- (at x <?=) =<< newId (x ^. idName) (C.typeOf $ typeOf x)
  Bind x' <$> e
crushPat (S.TupleP _ xs) = go xs []
  where
    go [] acc e = do
      acc <- pure $ reverse acc
      Unpack (Con ("Tuple" <> length acc ^. toText) $ map C.typeOf acc) acc <$> e
    go (p : ps) acc e = do
      x <- newId "p" (C.typeOf $ typeOf p)
      go ps (x : acc) $ do
        clause <- crushPat p e
        pure $ Match (Atom $ Var x) (clause :| [])

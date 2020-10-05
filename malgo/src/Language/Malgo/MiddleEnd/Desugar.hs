{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Malgo.MiddleEnd.Desugar
  ( desugar,
  )
where

import qualified Data.Set as Set
  ( fromList,
    singleton,
    toList,
  )
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

boolValue :: MonadUniq m => Bool -> m (Exp (Id C.Type))
boolValue x =
  runDef $
    fmap Atom $
      let_ boolType $
        if x
          then Pack boolType trueC []
          else Pack boolType falseC []
  where
    boolType = SumT $ Set.fromList [trueC, falseC]
    trueC = Con "True" []
    falseC = Con "False" []

toExp ::
  (MonadState (Map (Id Type) (Id C.Type)) m, MonadUniq m, MonadFail m) =>
  S.Expr (Id Type) ->
  m (Exp (Id C.Type))
toExp (S.Var _ x) = Atom . Var <$> findVar x
toExp (S.Int _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Int64 $ fromInteger x]
  where
    ty = SumT $ Set.singleton con
    con = Con "Int" [Int64T]
toExp (S.Float _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Double x]
  where
    ty = SumT $ Set.singleton con
    con = Con "Float" [DoubleT]
toExp (S.Bool _ x) = boolValue x
toExp (S.Char _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Char x]
  where
    ty = SumT $ Set.singleton con
    con = Con "Char" [CharT]
toExp (S.String _ x) = runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ String x]
  where
    ty = SumT $ Set.singleton con
    con = Con "String" [StringT]
toExp (S.Tuple _ xs) = runDef $ do
  vs <- traverse (bind <=< toExp) xs
  let con = Con ("Tuple" <> length xs ^. toText) $ map C.typeOf vs
  let ty = SumT $ Set.singleton con
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
  ps' <- traverse ((\p -> newId (C.typeOf p) (p ^. idName)) . fst) ps
  e <- do
    zipWithM_ (\(p, _) p' -> modify (at p ?~ p')) ps ps'
    toExp e
  Atom <$> let_ (map C.typeOf ps' :-> C.typeOf e) (Fun ps' e)
toExp (S.Seq _ e1 e2) = runDef $ do
  _ <- bind =<< toExp e1
  toExp e2
toExp (S.Let _ (S.ValDec _ a _ v) e) = runDef $ do
  Var v <- cast (C.typeOf a) =<< toExp v
  modify $ at a ?~ v
  toExp e
toExp (S.Let _ (S.ExDec _ prim _ primName) e) = case C.typeOf $ prim ^. idMeta of
  ta :-> tb -> runDef $ do
    ps <- traverse (newId ?? "a") ta
    Var prim' <- let_ (ta :-> tb) $ Fun ps $ PrimCall primName (ta :-> tb) (map Var ps)
    modify $ at prim ?~ prim'
    toExp e
  _ -> bug Unreachable
toExp (S.Let _ (S.FunDec fs) e) = do
  for_ fs $ \(_, f, _, _, _) -> do
    f' <- newId (C.typeOf f) (f ^. idName)
    modify (at f ?~ f')
  Let <$> traverse toFun fs <*> toExp e
  where
    toFun (_, f@(C.typeOf -> _ :-> r), ps, _, body) = do
      ps' <- traverse ((\p -> newId (C.typeOf p) (p ^. idName)) . fst) ps
      body <- runDef $ do
        zipWithM_ (\(p, _) p' -> modify (at p ?~ p')) ps ps'
        Atom <$> (cast r =<< toExp body)
      f <- findVar f
      pure (f, Fun ps' body)
    toFun _ = bug Unreachable
toExp (S.If _ c t f) = do
  c <- toExp c
  t <- Unpack (Con "True" []) [] <$> toExp t
  f <- Unpack (Con "False" []) [] <$> toExp f
  pure $ Match c (t :| [f])
toExp (S.BinOp _ opr x y) = case opr of
  S.Add -> arithOp (Con "Int" [Int64T])
  S.Sub -> arithOp (Con "Int" [Int64T])
  S.Mul -> arithOp (Con "Int" [Int64T])
  S.Div -> arithOp (Con "Int" [Int64T])
  S.Mod -> arithOp (Con "Int" [Int64T])
  S.FAdd -> arithOp (Con "Float" [DoubleT])
  S.FSub -> arithOp (Con "Float" [DoubleT])
  S.FMul -> arithOp (Con "Float" [DoubleT])
  S.FDiv -> arithOp (Con "Float" [DoubleT])
  S.Eq -> equalOp
  S.Neq -> equalOp
  S.Lt -> compareOp
  S.Gt -> compareOp
  S.Le -> compareOp
  S.Ge -> compareOp
  S.And -> runDef $ do
    lexp <- toExp x
    rexp <- toExp y
    whenFalse <- Unpack (Con "False" []) [] . Atom <$> bind lexp
    whenTrue <- Bind <$> newId (C.typeOf lexp) "lexp" <*> (Atom <$> bind rexp)
    pure $ Match lexp (whenFalse :| [whenTrue])
  S.Or -> runDef $ do
    lexp <- toExp x
    rexp <- toExp y
    whenTrue <- Unpack (Con "True" []) [] . Atom <$> bind lexp
    whenFalse <- Bind <$> newId (C.typeOf lexp) "lexp" <*> (Atom <$> bind rexp)
    pure $ Match lexp (whenTrue :| [whenFalse])
  where
    arithOp con = runDef $ do
      lexp <- toExp x
      rexp <- toExp y
      [lval] <- destruct lexp con
      [rval] <- destruct rexp con
      result <- bind $ BinOp opr lval rval
      let ty = SumT $ Set.singleton con
      Atom <$> let_ ty (Pack ty con [result])
    compareOp = runDef $ do
      lexp <- toExp x
      rexp <- toExp y
      case C.typeOf lexp of
        SumT (toList -> [con]) | con == Con "Int" [Int64T] || con == Con "Float" [DoubleT] -> do
          [lval] <- destruct lexp con
          [rval] <- destruct rexp con
          pure $ BinOp opr lval rval
        _ -> bug Unreachable
    equalOp = do
      lexp <- toExp x
      rexp <- toExp y
      case C.typeOf lexp of
        SumT (Set.toList -> [Con "Int" [Int64T]]) -> runDef $ do
          [lval] <- destruct lexp (Con "Int" [Int64T])
          [rval] <- destruct rexp (Con "Int" [Int64T])
          pure $ BinOp opr lval rval
        SumT (Set.toList -> [Con "Float" [DoubleT]]) -> runDef $ do
          [lval] <- destruct lexp (Con "Float" [DoubleT])
          [rval] <- destruct rexp (Con "Float" [DoubleT])
          pure $ BinOp opr lval rval
        SumT (Set.toList -> [Con "Char" [CharT]]) -> runDef $ do
          [lval] <- destruct lexp (Con "Char" [CharT])
          [rval] <- destruct rexp (Con "Char" [CharT])
          pure $ BinOp opr lval rval
        SumT (Set.toList -> [Con "False" [], Con "True" []]) -> runDef $ do
          lval <- bind lexp
          rval <- bind rexp
          -- lval == rval
          -- -> if lval then (if rval then true else false) else (if rval then false else true)
          -- -> if lval then rval else (if rval then false else true)
          -- -> if lval then rval else (if rval then lval else true)
          let whenTrue = Unpack (Con "True" []) [] (Atom rval)
          whenFalse <- do
            lvalHole <- newId (C.typeOf lval) "lval"
            rvalHole <- newId (C.typeOf rval) "rval"
            whenFalse <- boolValue True
            pure $
              Bind lvalHole $
                Match
                  (Atom rval)
                  (Unpack (Con "True" []) [] (Atom lval) :| [Bind rvalHole whenFalse])
          pure $ Match (Atom lval) (whenTrue :| [whenFalse])
        _ -> errorDoc $ "not implemented:" <+> pPrint opr $$ pPrint lexp $$ pPrint rexp
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
  x' <- newId (C.typeOf $ typeOf x) (x ^. idName)
  modify $ at x ?~ x'
  Bind x' <$> e
crushPat (S.TupleP _ xs) = go xs []
  where
    go [] acc e = do
      acc <- pure $ reverse acc
      Unpack (Con ("Tuple" <> length acc ^. toText) $ map C.typeOf acc) acc <$> e
    go (p : ps) acc e = do
      x <- newId (C.typeOf $ typeOf p) "p"
      go ps (x : acc) $ do
        clause <- crushPat p e
        pure $ Match (Atom $ Var x) (clause :| [])

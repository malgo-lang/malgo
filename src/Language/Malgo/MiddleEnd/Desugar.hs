{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Malgo.MiddleEnd.Desugar
  ( Desugar,
  )
where

import qualified Data.Set as Set (fromList, singleton, toList)
import qualified Data.Text as T
import Language.Malgo.IR.Core
import qualified Language.Malgo.IR.Syntax as S
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType
import Language.Malgo.TypeRep.Type hiding ((:->))
import Text.PrettyPrint (($$))

data Desugar

instance Pass Desugar (S.Expr (Id Type)) (Exp (Id CType)) where
  passName = "Desugar"
  isDump = dumpDesugar
  trans e = evalStateT ?? mempty $ toExp e

findVar :: (MonadFail m, MonadState (IdMap Type (Id CType)) m) => Id Type -> m (Id CType)
findVar v = do
  Just v <- gets (view (at v))
  pure v

runDef :: Functor f => WriterT (Endo a) f a -> f a
runDef m = uncurry (flip appEndo) <$> runWriterT m

boolValue :: MonadUniq m => Bool -> m (Exp (Id CType))
boolValue x =
  runDef $ fmap Atom $ let_ boolType $ if x then Pack boolType trueC [] else Pack boolType falseC []
  where
    boolType = SumT $ Set.fromList [trueC, falseC]
    trueC = Con "True" []
    falseC = Con "False" []

toExp :: (MonadState (IdMap Type (Id CType)) m, MonadUniq m, MonadFail m) => S.Expr (Id Type) -> m (Exp (Id CType))
toExp (S.Var _ x) =
  Atom . Var <$> findVar x
toExp (S.Int _ x) =
  runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Int x]
  where
    ty = SumT $ Set.singleton con
    con = Con "Int" [IntT]
toExp (S.Float _ x) =
  runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Float x]
  where
    ty = SumT $ Set.singleton con
    con = Con "Float" [FloatT]
toExp (S.Bool _ x) = boolValue x
toExp (S.Char _ x) =
  runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ Char x]
  where
    ty = SumT $ Set.singleton con
    con = Con "Char" [CharT]
toExp (S.String _ x) =
  runDef $ fmap Atom $ let_ ty $ Pack ty con [Unboxed $ String x]
  where
    ty = SumT $ Set.singleton con
    con = Con "String" [StringT]
toExp (S.Tuple _ xs) = runDef $ do
  vs <- traverse (bind <=< toExp) xs
  let con = Con ("Tuple" <> T.pack (show $ length xs)) $ map cTypeOf vs
  let ty = SumT $ Set.singleton con
  runDef $ fmap Atom $ let_ ty $ Pack ty con vs
toExp (S.Array _ (x :| xs)) = runDef $ do
  x <- bind =<< toExp x
  arr <- let_ (ArrayT $ cTypeOf x) $ Array x $ Unboxed $ Int $ fromIntegral $ length xs + 1
  ifor_ xs $ \i v -> do
    v <- bind =<< toExp v
    bind $ ArrayWrite arr (Unboxed $ Int $ fromIntegral $ i + 1) v
  pure $ Atom arr
toExp (S.MakeArray _ a n) = runDef $ do
  a <- bind =<< toExp a
  n <- bind =<< toExp n
  [n] <- destruct (Atom n) $ Con "Int" [IntT]
  Atom <$> let_ (ArrayT $ cTypeOf a) (Array a n)
toExp (S.ArrayRead _ a i) = runDef $ do
  a <- bind =<< toExp a
  i <- bind =<< toExp i
  [i] <- destruct (Atom i) $ Con "Int" [IntT]
  Atom <$> bind (ArrayRead a i)
toExp (S.ArrayWrite _ a i x) = runDef $ do
  a <- bind =<< toExp a
  i <- bind =<< toExp i
  case cTypeOf a of
    ArrayT ty -> do
      x <- cast ty =<< toExp x
      [i] <- destruct (Atom i) $ Con "Int" [IntT]
      Atom <$> bind (ArrayWrite a i x)
    _ -> bug Unreachable
toExp (S.Call _ f xs) = runDef $ do
  f <- bind =<< toExp f
  case cTypeOf f of
    ps :-> _ -> Call f <$> zipWithM (\x p -> cast p =<< toExp x) xs ps
    _ -> bug Unreachable
toExp (S.Fn _ ps e) = runDef $ do
  ps' <- traverse ((\p -> newId (cTypeOf p) (p ^. idName)) . fst) ps
  e <- do
    zipWithM_ (\(p, _) p' -> modify (at p ?~ p')) ps ps'
    toExp e
  Atom <$> let_ (map cTypeOf ps' :-> cTypeOf e) (Fun ps' e)
toExp (S.Seq _ e1 e2) = runDef $ do
  _ <- bind =<< toExp e1
  toExp e2
toExp (S.Let _ (S.ValDec _ a _ v) e) = runDef $ do
  Var v <- cast (cTypeOf a) =<< toExp v
  modify $ at a ?~ v
  toExp e
toExp (S.Let _ (S.ExDec _ prim _ primName) e) =
  case cTypeOf $ prim ^. idMeta of
    ta :-> tb -> runDef $ do
      ps <- traverse (newId ?? "a") ta
      Var prim' <-
        let_ (ta :-> tb) $ Fun ps $
          PrimCall (T.pack primName) (ta :-> tb) (map Var ps)
      modify $ at prim ?~ prim'
      toExp e
    _ -> bug Unreachable
toExp (S.Let _ (S.FunDec fs) e) = do
  for_ fs $ \(_, f, _, _, _) -> do
    f' <- newId (cTypeOf f) (f ^. idName)
    modify (at f ?~ f')
  Let <$> traverse toFun fs <*> toExp e
  where
    toFun (_, f@(cTypeOf -> _ :-> r), ps, _, body) = do
      ps' <- traverse ((\p -> newId (cTypeOf p) (p ^. idName)) . fst) ps
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
toExp (S.BinOp _ opr x y) =
  case opr of
    S.Add -> arithOp (Con "Int" [IntT])
    S.Sub -> arithOp (Con "Int" [IntT])
    S.Mul -> arithOp (Con "Int" [IntT])
    S.Div -> arithOp (Con "Int" [IntT])
    S.Mod -> arithOp (Con "Int" [IntT])
    S.FAdd -> arithOp (Con "Float" [FloatT])
    S.FSub -> arithOp (Con "Float" [FloatT])
    S.FMul -> arithOp (Con "Float" [FloatT])
    S.FDiv -> arithOp (Con "Float" [FloatT])
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
      whenTrue <- Bind <$> newId (cTypeOf lexp) "lexp" <*> (Atom <$> bind rexp)
      pure $ Match lexp (whenFalse :| [whenTrue])
    S.Or -> runDef $ do
      lexp <- toExp x
      rexp <- toExp y
      whenTrue <- Unpack (Con "True" []) [] . Atom <$> bind lexp
      whenFalse <- Bind <$> newId (cTypeOf lexp) "lexp" <*> (Atom <$> bind rexp)
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
      case cTypeOf lexp of
        SumT (toList -> [con])
          | con == Con "Int" [IntT] || con == Con "Float" [FloatT] -> do
            [lval] <- destruct lexp con
            [rval] <- destruct rexp con
            pure $ BinOp opr lval rval
        _ -> bug Unreachable
    equalOp = do
      lexp <- toExp x
      rexp <- toExp y
      case cTypeOf lexp of
        SumT (Set.toList -> [Con "Int" [IntT]]) -> runDef $ do
          [lval] <- destruct lexp (Con "Int" [IntT])
          [rval] <- destruct rexp (Con "Int" [IntT])
          pure $ BinOp opr lval rval
        SumT (Set.toList -> [Con "Float" [FloatT]]) -> runDef $ do
          [lval] <- destruct lexp (Con "Float" [FloatT])
          [rval] <- destruct rexp (Con "Float" [FloatT])
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
            lvalHole <- newId (cTypeOf lval) "lval"
            rvalHole <- newId (cTypeOf rval) "rval"
            whenFalse <- boolValue True
            pure $ Bind lvalHole $ Match (Atom rval) (Unpack (Con "True" []) [] (Atom lval) :| [Bind rvalHole whenFalse])
          pure $ Match (Atom lval) (whenTrue :| [whenFalse])
        _ -> errorDoc $ "not implemented:" <+> pPrint opr $$ pPrint lexp $$ pPrint rexp
toExp (S.Match _ e cs) = do
  e <- toExp e
  cs <- traverse (\(p, v) -> crushPat p $ toExp v) cs
  pure $ Match e cs

crushPat ::
  (MonadUniq m, MonadState (IdMap Type (Id CType)) m) =>
  S.Pat (Id Type) ->
  m (Exp (Id CType)) ->
  m (Case (Id CType))
crushPat (S.VarP _ x) = \e -> do
  x' <- newId (cTypeOf $ typeOf x) (x ^. idName)
  modify $ at x ?~ x'
  Bind x' <$> e
crushPat (S.TupleP _ xs) = go xs []
  where
    go [] acc e = do
      acc <- pure $ reverse acc
      Unpack (Con ("Tuple" <> T.pack (show $ length acc)) $ map cTypeOf acc) acc <$> e
    go (p : ps) acc e = do
      x <- newId (cTypeOf $ typeOf p) "p"
      go ps (x : acc) $ do
        clause <- crushPat p e
        pure $ Match (Atom $ Var x) (clause :| [])

let_ ::
  (MonadUniq m, MonadWriter (Endo (Exp (Id a))) m) =>
  a ->
  Obj (Id a) ->
  m (Atom (Id a))
let_ otype obj = do
  x <- newId otype "$let"
  tell $ Endo $ \e -> Let [(x, obj)] e
  pure (Var x)

destruct ::
  (MonadUniq m, MonadWriter (Endo (Exp (Id CType))) m) =>
  Exp (Id CType) ->
  Con ->
  m [Atom (Id CType)]
destruct val con@(Con _ ts) = do
  vs <- traverse (newId ?? "$p") ts
  tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
  pure $ map Var vs

bind :: (MonadUniq m, MonadWriter (Endo (Exp (Id CType))) m) => Exp (Id CType) -> m (Atom (Id CType))
bind (Atom a) = pure a
bind v = do
  x <- newId (cTypeOf v) "$d"
  tell $ Endo $ \e -> Match v (Bind x e :| [])
  pure (Var x)

cast ::
  (MonadUniq f, MonadWriter (Endo (Exp (Id CType))) f) =>
  CType ->
  Exp (Id CType) ->
  f (Atom (Id CType))
cast ty e
  | ty == cTypeOf e = bind e
  | otherwise = do
    v <- bind e
    x <- newId ty "$cast"
    tell $ Endo $ \e -> Match (Cast ty v) (Bind x e :| [])
    pure (Var x)

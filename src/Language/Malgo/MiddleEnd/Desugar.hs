{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Malgo.MiddleEnd.Desugar
  ( Desugar,
  )
where

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

def :: (MonadUniq m, MonadWriter (Endo (Exp (Id CType))) m) => Exp (Id CType) -> m (Atom (Id CType))
def (Atom a) = pure a
def v = do
  x <- newId (cTypeOf v) "$d"
  tell $ Endo $ \e -> Match v [Bind x e]
  pure (Var x)

cast ::
  (MonadUniq f, MonadWriter (Endo (Exp (Id CType))) f) =>
  CType ->
  Atom (Id CType) ->
  f (Atom (Id CType))
cast ty v
  | ty == cTypeOf v = pure v
  | otherwise = do
    x <- newId ty "$cast"
    tell $ Endo $ \e -> Match (Cast ty v) [Bind x e]
    pure (Var x)

runDef :: Functor f => WriterT (Endo a) f a -> f a
runDef m = uncurry (flip appEndo) <$> runWriterT m

boolValue :: MonadUniq m => Bool -> m (Exp (Id CType))
boolValue x =
  allocObj (PackT [trueC, falseC]) $ if x then Pack boolType trueC [] else Pack boolType falseC []
  where
    boolType = PackT [trueC, falseC]
    trueC = Con "True" []
    falseC = Con "False" []

toExp :: (MonadState (IdMap Type (Id CType)) m, MonadUniq m, MonadFail m) => S.Expr (Id Type) -> m (Exp (Id CType))
toExp (S.Var _ x) =
  Atom . Var <$> findVar x
toExp (S.Int _ x) =
  allocObj (PackT [con]) $ Pack (PackT [con]) con [Unboxed $ Int x]
  where
    con = Con "Int" [IntT]
toExp (S.Float _ x) =
  allocObj (PackT [con]) $ Pack (PackT [con]) con [Unboxed $ Float x]
  where
    con = Con "Float" [FloatT]
toExp (S.Bool _ x) = boolValue x
toExp (S.Char _ x) =
  allocObj (PackT [con]) $ Pack (PackT [con]) con [Unboxed $ Char x]
  where
    con = Con "Char" [CharT]
toExp (S.String _ x) =
  allocObj (PackT [con]) $ Pack (PackT [con]) con [Unboxed $ String x]
  where
    con = Con "String" [StringT]
toExp (S.Tuple _ xs) = runDef $ do
  vs <- traverse (def <=< toExp) xs
  let con = Con ("Tuple" <> T.pack (show $ length xs)) $ map cTypeOf vs
  allocObj (PackT [con]) $ Pack (PackT [con]) con vs
toExp (S.Array _ (x :| xs)) = runDef $ do
  x <- def =<< toExp x
  let_ "array" (ArrayT $ cTypeOf x) (Array x $ Unboxed (Int $ fromIntegral $ length xs + 1)) $ \arr -> do
    ifor_ xs $ \i v -> do
      v <- def =<< toExp v
      def (ArrayWrite (Var arr) (Unboxed (Int $ fromIntegral $ i + 1)) v)
    pure $ Atom $ Var arr
toExp (S.MakeArray _ a n) = runDef $ do
  a <- def =<< toExp a
  n <- def =<< toExp n
  destruct (Atom n) (Con "Int" [IntT]) $ \[n] -> allocObj (ArrayT (cTypeOf a)) $ Array a $ Var n
toExp (S.ArrayRead _ a i) = runDef $ do
  a <- def =<< toExp a
  i <- def =<< toExp i
  destruct (Atom i) (Con "Int" [IntT]) $ \[i] -> Atom <$> def (ArrayRead a $ Var i)
toExp (S.ArrayWrite _ a i x) = runDef $ do
  a <- def =<< toExp a
  i <- def =<< toExp i
  case cTypeOf a of
    ArrayT ty -> do
      x <- cast ty =<< def =<< toExp x
      destruct (Atom i) (Con "Int" [IntT]) $ \[i] -> Atom <$> def (ArrayWrite a (Var i) x)
    _ -> bug Unreachable
toExp (S.Call _ f xs) = runDef $ do
  f <- def =<< toExp f
  case cTypeOf f of
    ps :-> _ -> Call f <$> zipWithM (\x p -> cast p =<< def =<< toExp x) xs ps
    _ -> bug Unreachable
toExp (S.Fn _ ps e) = do
  ps' <- traverse ((\p -> newId (cTypeOf p) (p ^. idName)) . fst) ps
  e <- do
    zipWithM_ (\(p, _) p' -> modify (at p ?~ p')) ps ps'
    toExp e
  allocObj (map cTypeOf ps' :-> cTypeOf e) $ Fun ps' e
toExp (S.Seq _ e1 e2) = do
  e1 <- toExp e1
  bind e1 "hole" (cTypeOf e1) $ \_ -> toExp e2
toExp (S.Let _ (S.ValDec _ a _ v) e) = runDef $ do
  v <- cast (cTypeOf a) =<< def =<< toExp v
  bind (Atom v) (a ^. idName) (cTypeOf a) $ \v -> modify (at a ?~ v) >> toExp e
toExp (S.Let _ (S.ExDec _ prim _ primName) e) =
  case cTypeOf $ prim ^. idMeta of
    ta :-> tb -> do
      ps <- traverse (newId ?? "a") ta
      let_ "prim" (ta :-> tb) (Fun ps (PrimCall (T.pack primName) (ta :-> tb) (map Var ps))) $ \prim' -> do
        modify (at prim ?~ prim')
        toExp e
    _ -> bug Unreachable
toExp (S.Let _ (S.FunDec fs) e) = do
  traverse_ ?? fs $ \(_, f, _, _, _) -> do
    f' <- newId (cTypeOf f) (f ^. idName)
    modify (at f ?~ f')
  fs <- traverse ?? fs $ \(_, f@(cTypeOf -> _ :-> r), ps, _, body) -> do
    ps' <- traverse ((\p -> newId (cTypeOf p) (p ^. idName)) . fst) ps
    body <- do
      zipWithM_ (\(p, _) p' -> modify (at p ?~ p')) ps ps'
      runDef $ Atom <$> (cast r =<< def =<< toExp body)
    f <- findVar f
    pure (f, Fun ps' body)
  e' <- toExp e
  pure $ Let fs e'
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
      whenFalse <- Unpack (Con "False" []) [] . Atom <$> def lexp
      whenTrue <- Bind <$> newId (cTypeOf lexp) "lexp" <*> (Atom <$> def rexp)
      pure $ Match lexp (whenFalse :| [whenTrue])
    S.Or -> runDef $ do
      lexp <- toExp x
      rexp <- toExp y
      whenTrue <- Unpack (Con "True" []) [] . Atom <$> def lexp
      whenFalse <- Bind <$> newId (cTypeOf lexp) "lexp" <*> (Atom <$> def rexp)
      pure $ Match lexp (whenTrue :| [whenFalse])
  where
    arithOp con = do
      lexp <- toExp x
      rexp <- toExp y
      destruct lexp con $ \[lval] ->
        destruct rexp con $ \[rval] -> do
          result <- def $ BinOp opr (Var lval) (Var rval)
          allocObj (PackT [con]) $ Pack (PackT [con]) con [result]
    compareOp = do
      lexp <- toExp x
      rexp <- toExp y
      case cTypeOf lexp of
        PackT (toList -> [con])
          | con == Con "Int" [IntT] || con == Con "Float" [FloatT] ->
            destruct lexp con $ \[lval] ->
              destruct rexp con $ \[rval] ->
                pure $ BinOp opr (Var lval) (Var rval)
        _ -> bug Unreachable
    equalOp = do
      lexp <- toExp x
      rexp <- toExp y
      case cTypeOf lexp of
        PackT [Con "Int" [IntT]] ->
          destruct lexp (Con "Int" [IntT]) $ \[lval] ->
            destruct rexp (Con "Int" [IntT]) $ \[rval] ->
              pure $ BinOp opr (Var lval) (Var rval)
        PackT [Con "Float" [FloatT]] ->
          destruct lexp (Con "Float" [FloatT]) $ \[lval] ->
            destruct rexp (Con "Float" [FloatT]) $ \[rval] ->
              pure $ BinOp opr (Var lval) (Var rval)
        PackT [Con "Char" [CharT]] ->
          destruct lexp (Con "Char" [CharT]) $ \[lval] ->
            destruct rexp (Con "Char" [CharT]) $ \[rval] ->
              pure $ BinOp opr (Var lval) (Var rval)
        PackT [Con "False" [], Con "True" []] -> runDef $ do
          lval <- def lexp
          rval <- def rexp
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

crushPat :: (MonadUniq m, MonadState (IdMap Type (Id CType)) m) => S.Pat (Id Type) -> m (Exp (Id CType)) -> m (Case (Id CType))
crushPat (S.VarP x) = \e -> do
  x' <- newId (cTypeOf $ typeOf x) (x ^. idName)
  modify $ at x ?~ x'
  Bind x' <$> e
crushPat (S.TupleP xs) = go xs []
  where
    go [] acc e = do
      acc <- pure $ reverse acc
      Unpack (Con ("Tuple" <> T.pack (show $ length acc)) $ map cTypeOf acc) acc <$> e
    go (p : ps) acc e = do
      x <- newId (cTypeOf $ typeOf p) "p"
      go ps (x : acc) $ do
        clause <- crushPat p e
        pure $ Match (Atom $ Var x) [clause]

let_ ::
  MonadUniq m =>
  String ->
  CType ->
  Obj (Id CType) ->
  (Id CType -> WriterT (Endo (Exp (Id CType))) m (Exp (Id CType))) ->
  m (Exp (Id CType))
let_ name otype o body = do
  v <- newId otype name
  body <- runDef $ body v
  pure $ Let [(v, o)] body

allocObj ::
  MonadUniq m =>
  CType ->
  Obj (Id CType) ->
  m (Exp (Id CType))
allocObj otype o = let_ "allocObj" otype o $ pure . Atom . Var

destruct ::
  MonadUniq m =>
  Exp (Id CType) ->
  Con ->
  ( [Id CType] ->
    WriterT (Endo (Exp (Id CType))) m (Exp (Id CType))
  ) ->
  m (Exp (Id CType))
destruct e con@(Con _ ts) body = do
  vs <- traverse (newId ?? "$p") ts
  body <- runDef $ body vs
  pure $ Match e (Unpack con vs body :| [])

bind ::
  MonadUniq m =>
  Exp (Id a) ->
  String ->
  a ->
  (Id a -> WriterT (Endo (Exp (Id a))) m (Exp (Id a))) ->
  m (Exp (Id a))
bind e name ty body = do
  x <- newId ty name
  body <- runDef $ body x
  pure $ Match e (Bind x body :| [])

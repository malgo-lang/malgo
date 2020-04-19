{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.Malgo.MiddleEnd.Desugar (Desugar) where

import qualified Data.Text                    as T
import           Language.Malgo.Id
import           Language.Malgo.IR.Core
import qualified Language.Malgo.IR.Syntax     as S
import           Language.Malgo.Monad
import           Language.Malgo.Pass
import           Language.Malgo.Prelude
import           Language.Malgo.TypeRep.CType
import           Language.Malgo.TypeRep.Type  hiding ((:->))

data Desugar

instance Pass Desugar (S.Expr (Id Type)) (Exp (Id CType)) where
  passName = "Desugar"
  isDump = const False -- TODO: dumpDesugar
  trans e = evalStateT ?? mempty $ toExp e

newTmp :: MonadUniq m => CType -> m (Id CType)
newTmp t = newId t "$d"

findVar :: (MonadFail m, MonadState (IdMap Type (Id CType)) m) => Id Type -> m (Id CType)
findVar v = do
  Just v' <- gets (view (at v))
  pure v'

def :: (MonadUniq m, MonadWriter (Endo (Exp (Id CType))) m) => Exp (Id CType) -> m (Atom (Id CType))
def (Atom x) = pure x
def v = do
  x <- newTmp (cTypeOf v)
  tell $ Endo $ \e -> Match v [] (x, e)
  pure (Var x)

runDef :: Functor f => WriterT (Endo a) f a -> f a
runDef m = uncurry (flip appEndo) <$> runWriterT m

toExp :: (MonadState (IdMap Type (Id CType)) m, MonadUniq m, MonadFail m) => S.Expr (Id Type) -> m (Exp (Id CType))
toExp (S.Var _ x) =
  Atom . Var <$> findVar x
toExp (S.Int _ x) =
  let_ (Pack con [Unboxed (Int x)]) (PackT [con]) $ pure . Atom . Var
  where con = Con "Int" [IntT]
toExp (S.Float _ x) =
  let_ (Pack con [Unboxed (Float x)]) (PackT [con]) $ pure . Atom . Var
  where con = Con "Float" [FloatT]
toExp (S.Bool _ x) =
  let_ (if x then Pack trueC [] else Pack falseC []) (PackT [trueC, falseC]) $ pure . Atom . Var
  where trueC = Con "True" []
        falseC = Con "False" []
toExp (S.Char _ x) =
  let_ (Pack con [Unboxed $ Char x]) (PackT [con]) $ pure . Atom . Var
  where con = Con "Char" [CharT]
toExp (S.String _ x) =
  let_ (Pack con [Unboxed $ String x]) (PackT [con]) $ pure . Atom . Var
  where con = Con "String" [StringT]
toExp (S.Tuple _ xs) = runDef $ do
  vs <- traverse (def <=< toExp) xs
  let con = Con ("Tuple" <> T.pack (show $ length xs)) $ map cTypeOf vs
  let_ (Pack con vs) (PackT [con]) $ pure . Atom . Var
toExp (S.Array _ (x :| xs)) = runDef $ do
  x' <- def =<< toExp x
  let_ (Array x' $ Unboxed (Int $ fromIntegral $ length xs + 1)) (ArrayT $ cTypeOf x') $ \arr -> do
    ifor_ xs $ \i v -> do
      v' <- def =<< toExp v
      def (ArrayWrite (Var arr) (Unboxed (Int $ fromIntegral $ i + 1)) v')
    pure $ Atom $ Var arr
toExp (S.MakeArray _ a n) = runDef $ do
  a' <- def =<< toExp a
  n' <- def =<< toExp n
  v <- newTmp $ ArrayT (cTypeOf a')
  match (Atom n') [(Con "Int" [IntT], \[n''] -> pure $ Let [(v, Array a' $ Var n'')] $ Atom a')] (\_ -> pure Undefined)
toExp (S.ArrayRead _ a i) = runDef $ do
  a' <- def =<< toExp a
  i' <- def =<< toExp i
  match (Atom i') [(Con "Int" [IntT], \[i''] -> Atom <$> def (ArrayRead a' $ Var i''))] (\_ -> pure Undefined)
toExp (S.ArrayWrite _ a i x) = runDef $ do
  a' <- def =<< toExp a
  i' <- def =<< toExp i
  x' <- def =<< toExp x
  match (Atom i') [(Con "Int" [IntT], \[i''] -> Atom <$> def (ArrayWrite a' (Var i'') x'))] (\_ -> pure Undefined)
toExp (S.Call _ f xs) = runDef $ do
  Var f' <- def =<< toExp f
  xs' <- traverse (def <=< toExp) xs
  let con = Con ("Tuple" <> T.pack (show $ length xs)) $ map cTypeOf xs'
  let_ (Pack con xs') (PackT [con]) $ \arg -> pure $ Call f' [Var arg]
toExp (S.Fn _ ps e) = do
  let con = Con ("Tuple" <> T.pack (show $ length ps)) $ map (cTypeOf . fst) ps
  paramTuple <- newTmp $ PackT [con]

  e' <- match (Atom $ Var paramTuple) [(con, \ps' -> do
    zipWithM_ (\(p, _) p' -> modify (set (at p) (Just p'))) ps ps'
    toExp e
    )] (\_ -> pure Undefined)

  let_ (Fun [paramTuple] e') (PackT [con] :-> cTypeOf e') $ pure . Atom . Var
toExp (S.Seq _ e1 e2) = do
  e1' <- toExp e1
  match e1' [] (\_ -> toExp e2)
toExp (S.Let _ (S.ValDec _ a _ v) e) = do
  v' <- toExp v
  match v' [] (\vId -> modify (set (at a) (Just vId)) >> toExp e)
toExp (S.Let _ (S.ExDec _ prim _ primName) e) = do
  case cTypeOf $ prim ^. idMeta of
    ta :-> tb -> do
      a <- newTmp ta
      let funBody = PrimCall (T.pack primName) (ta :-> tb) [Var a]
      let_ (Fun [a] funBody) (ta :-> tb) $ \prim' -> do
        modify (set (at prim) (Just prim'))
        toExp e
    _ -> bug Unreachable
toExp (S.Let _ (S.FunDec fs) e) = do
  zipWithM_ (\f -> modify . set (at (f ^. _2)) . Just) fs =<< traverse (newTmp . cTypeOf . view (_2 . idMeta)) fs
  fs' <- traverse ?? fs $ \(_, f, ps, _, body) -> do
    let con = Con ("Tuple" <> T.pack (show $ length ps)) (map (cTypeOf . fst) ps)
    paramTuple <- newTmp $ PackT [con]
    body' <- match (Atom $ Var paramTuple) [(con, \ps' -> do
      zipWithM_ (\(p, _) p' -> modify (set (at p) (Just p'))) ps ps'
      toExp body)] (\_ -> pure Undefined)
    f' <- findVar f
    pure $ (f', Fun [paramTuple] body')
  e' <- toExp e
  pure $ Let fs' e'
toExp (S.If _ c t f) = do
  c' <- toExp c
  match c' [(Con "True" [], \_ -> toExp t), (Con "False" [], \_ -> toExp f)] (\_ -> pure Undefined)
toExp (S.BinOp _ o x y) =
  case o of
    S.Add -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "IntT" [IntT]) "+" (IntT :-> IntT :-> IntT)
    S.Sub -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "IntT" [IntT]) "-" (IntT :-> IntT :-> IntT)
    S.Mul -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "IntT" [IntT]) "*" (IntT :-> IntT :-> IntT)
    S.Div -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "IntT" [IntT]) "/" (IntT :-> IntT :-> IntT)
    S.Mod -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "IntT" [IntT]) "%" (IntT :-> IntT :-> IntT)
    S.FAdd -> arithOpPrim (Con "Float" [FloatT]) (Con "Float" [FloatT]) (Con "FloatT" [FloatT]) "+." (FloatT :-> FloatT :-> FloatT)
    S.FSub -> arithOpPrim (Con "Float" [FloatT]) (Con "Float" [FloatT]) (Con "FloatT" [FloatT]) "-." (FloatT :-> FloatT :-> FloatT)
    S.FMul -> arithOpPrim (Con "Float" [FloatT]) (Con "Float" [FloatT]) (Con "FloatT" [FloatT]) "*." (FloatT :-> FloatT :-> FloatT)
    S.FDiv -> arithOpPrim (Con "Float" [FloatT]) (Con "Float" [FloatT]) (Con "FloatT" [FloatT]) "/." (FloatT :-> FloatT :-> FloatT)
    S.Eq -> compareOpPrim "=="
    S.Neq -> compareOpPrim "<>"
    S.Lt -> compareOpPrim "<"
    S.Gt -> compareOpPrim ">"
    S.Le -> compareOpPrim "<="
    S.Ge -> compareOpPrim ">="
    S.And -> do
      lexp <- toExp x
      rexp <- toExp y
      match lexp
        [(Con "False" [], \_ -> Atom <$> def lexp)]
        (\_ -> Atom <$> def rexp)
    S.Or -> do
      lexp <- toExp x
      rexp <- toExp y
      match lexp
        [(Con "True" [], \_ -> Atom <$> def lexp)]
        (\_ -> Atom <$> def rexp)
  where
    arithOpPrim lcon rcon resultCon primName primType = do
      lexp <- toExp x
      rexp <- toExp y
      match lexp [(lcon, \[lval] ->
        match rexp [(rcon, \[rval] ->
          match (PrimCall primName primType [Var lval, Var rval]) [] $ \result ->
            let_ (Pack resultCon [Var result]) (PackT [resultCon]) $ pure . Atom . Var)]
          (\_ -> pure Undefined))]
        (\_ -> pure Undefined)
    compareOpPrim primName = runDef $ do
      lval <- def =<< toExp x
      rval <- def =<< toExp y
      match (PrimCall primName (cTypeOf lval :-> cTypeOf rval :-> PackT [Con "True" [], Con "False" []]) [lval, rval]) [] $ pure . Atom . Var

let_ ::
  MonadUniq m
  => Obj (Id CType)
  -> CType
  -> (Id CType -> WriterT (Endo (Exp (Id CType))) m (Exp (Id CType)))
  -> m (Exp (Id CType))
let_ o otype body = do
  v <- newTmp otype
  body' <- runDef $ body v
  pure $ Let [(v, o)] body'

match ::
  MonadUniq m
  => Exp (Id CType)
  -> [(Con, [Id CType] -> WriterT (Endo (Exp (Id CType))) m (Exp (Id CType)))]
  -> (Id CType -> WriterT (Endo (Exp (Id CType))) m (Exp (Id CType)))
  -> m (Exp (Id CType))
match e ps d = do
  hole <- newTmp $ cTypeOf e
  ps' <- traverse ?? ps $ \(con@(Con _ ts), body) -> do
    vs <- traverse newTmp ts
    body' <- runDef $ body vs
    pure $ Unpack con vs body'
  d' <- runDef $ d hole
  pure $ Match e ps' (hole, d')

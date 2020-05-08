{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Language.Malgo.TypeRep.CType
import Language.Malgo.TypeRep.Type hiding ((:->))

data Desugar

instance Pass Desugar (S.Expr (Id Type)) (Exp (Id CType)) where
  passName = "Desugar"
  isDump = dumpDesugar
  trans e = evalStateT ?? mempty $ toExp e

findVar :: (MonadFail m, MonadState (IdMap Type (Id CType)) m) => Id Type -> m (Id CType)
findVar v = do
  Just v' <- gets (view (at v))
  pure v'

def :: (MonadUniq m, MonadWriter (Endo (Exp (Id CType))) m) => Exp (Id CType) -> m (Atom (Id CType))
def (Atom x) = pure x
def v = do
  x <- newId (cTypeOf v) "$d"
  tell $ Endo $ \e -> Match v [Bind x e]
  pure (Var x)

runDef :: Functor f => WriterT (Endo a) f a -> f a
runDef m = uncurry (flip appEndo) <$> runWriterT m

toExp :: (MonadState (IdMap Type (Id CType)) m, MonadUniq m, MonadFail m) => S.Expr (Id Type) -> m (Exp (Id CType))
toExp (S.Var _ x) =
  Atom . Var <$> findVar x
toExp (S.Int _ x) =
  let_ "int" (Pack con [Unboxed (Int x)]) (PackT [con]) $ pure . Atom . Var
  where
    con = Con "Int" [IntT]
toExp (S.Float _ x) =
  let_ "float" (Pack con [Unboxed (Float x)]) (PackT [con]) $ pure . Atom . Var
  where
    con = Con "Float" [FloatT]
toExp (S.Bool _ x) =
  let_ "bool" (if x then Pack trueC [] else Pack falseC []) (PackT [trueC, falseC]) $ pure . Atom . Var
  where
    trueC = Con "True" []
    falseC = Con "False" []
toExp (S.Char _ x) =
  let_ "char" (Pack con [Unboxed $ Char x]) (PackT [con]) $ pure . Atom . Var
  where
    con = Con "Char" [CharT]
toExp (S.String _ x) =
  let_ "string" (Pack con [Unboxed $ String x]) (PackT [con]) $ pure . Atom . Var
  where
    con = Con "String" [StringT]
toExp (S.Tuple _ xs) = runDef $ do
  vs <- traverse (def <=< toExp) xs
  let con = Con ("Tuple" <> T.pack (show $ length xs)) $ map cTypeOf vs
  let_ "tuple" (Pack con vs) (PackT [con]) $ pure . Atom . Var
toExp (S.Array _ (x :| xs)) = runDef $ do
  x' <- def =<< toExp x
  let_ "array" (Array x' $ Unboxed (Int $ fromIntegral $ length xs + 1)) (ArrayT $ cTypeOf x') $ \arr -> do
    ifor_ xs $ \i v -> do
      v' <- def =<< toExp v
      def (ArrayWrite (Var arr) (Unboxed (Int $ fromIntegral $ i + 1)) v')
    pure $ Atom $ Var arr
toExp (S.MakeArray _ a n) = runDef $ do
  a' <- def =<< toExp a
  n' <- def =<< toExp n
  v <- newId (ArrayT (cTypeOf a')) "array"
  match (Atom n') [(Right $ Con "Int" [IntT], \[n''] -> pure $ Let [(v, Array a' $ Var n'')] $ Atom $ Var v)]
toExp (S.ArrayRead _ a i) = runDef $ do
  a' <- def =<< toExp a
  i' <- def =<< toExp i
  match (Atom i') [(Right $ Con "Int" [IntT], \[i''] -> Atom <$> def (ArrayRead a' $ Var i''))]
toExp (S.ArrayWrite _ a i x) = runDef $ do
  a' <- def =<< toExp a
  i' <- def =<< toExp i
  x' <- def =<< toExp x
  match (Atom i') [(Right $ Con "Int" [IntT], \[i''] -> Atom <$> def (ArrayWrite a' (Var i'') x'))]
toExp (S.Call _ f xs) = runDef $ do
  Var f' <- def =<< toExp f
  xs' <- traverse (def <=< toExp) xs
  let con = Con ("Tuple" <> T.pack (show $ length xs)) $ map cTypeOf xs'
  let_ "args" (Pack con xs') (PackT [con]) $ \arg -> pure $ Call f' [Var arg]
toExp (S.Fn _ ps e) = do
  let con = Con ("Tuple" <> T.pack (show $ length ps)) $ map (cTypeOf . fst) ps
  paramTuple <- newId (PackT [con]) "param"
  e' <-
    match
      (Atom $ Var paramTuple)
      [ ( Right con,
          \ps' -> do
            zipWithM_ (\(p, _) p' -> modify (at p ?~ p')) ps ps'
            toExp e
        )
      ]
  let_ "fn" (Fun [paramTuple] e') (PackT [con] :-> cTypeOf e') $ pure . Atom . Var
toExp (S.Seq _ e1 e2) = do
  e1' <- toExp e1
  match e1' [(Left ("hole", cTypeOf e1'), \_ -> toExp e2)]
toExp (S.Let _ (S.ValDec _ a _ v) e) = do
  v' <- toExp v
  match v' [(Left (a ^. idName, cTypeOf v'), \[vId] -> modify (at a ?~ vId) >> toExp e)]
toExp (S.Let _ (S.ExDec _ prim _ primName) e) =
  case cTypeOf $ prim ^. idMeta of
    ta :-> tb -> do
      a <- newId ta "param"
      let funBody = PrimCall (T.pack primName) (ta :-> tb) [Var a]
      let_ "prim" (Fun [a] funBody) (ta :-> tb) $ \prim' -> do
        modify (at prim ?~ prim')
        toExp e
    _ -> bug Unreachable
toExp (S.Let _ (S.FunDec fs) e) = do
  traverse_ ?? fs $ \(_, f, _, _, _) -> do
    f' <- newId (cTypeOf f) (f ^. idName)
    modify (at f ?~ f')
  fs' <- traverse ?? fs $ \(_, f, ps, _, body) -> do
    let con = Con ("Tuple" <> T.pack (show $ length ps)) (map (cTypeOf . fst) ps)
    paramTuple <- newId (PackT [con]) "param"
    body' <-
      match
        (Atom $ Var paramTuple)
        [ ( Right con,
            \ps' -> do
              zipWithM_ (\(p, _) p' -> modify (at p ?~ p')) ps ps'
              toExp body
          )
        ]
    f' <- findVar f
    pure (f', Fun [paramTuple] body')
  e' <- toExp e
  pure $ Let fs' e'
toExp (S.If _ c t f) = do
  c' <- toExp c
  match c' [(Right $ Con "True" [], \_ -> toExp t), (Right $ Con "False" [], \_ -> toExp f)]
toExp (S.BinOp _ o x y) =
  case o of
    S.Add -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "Int" [IntT]) "+" (IntT :-> IntT :-> IntT)
    S.Sub -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "Int" [IntT]) "-" (IntT :-> IntT :-> IntT)
    S.Mul -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "Int" [IntT]) "*" (IntT :-> IntT :-> IntT)
    S.Div -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "Int" [IntT]) "/" (IntT :-> IntT :-> IntT)
    S.Mod -> arithOpPrim (Con "Int" [IntT]) (Con "Int" [IntT]) (Con "Int" [IntT]) "%" (IntT :-> IntT :-> IntT)
    S.FAdd -> arithOpPrim (Con "Float" [FloatT]) (Con "Float" [FloatT]) (Con "Float" [FloatT]) "+." (FloatT :-> FloatT :-> FloatT)
    S.FSub -> arithOpPrim (Con "Float" [FloatT]) (Con "Float" [FloatT]) (Con "Float" [FloatT]) "-." (FloatT :-> FloatT :-> FloatT)
    S.FMul -> arithOpPrim (Con "Float" [FloatT]) (Con "Float" [FloatT]) (Con "Float" [FloatT]) "*." (FloatT :-> FloatT :-> FloatT)
    S.FDiv -> arithOpPrim (Con "Float" [FloatT]) (Con "Float" [FloatT]) (Con "Float" [FloatT]) "/." (FloatT :-> FloatT :-> FloatT)
    S.Eq -> compareOpPrim "=="
    S.Neq -> compareOpPrim "<>"
    S.Lt -> compareOpPrim "<"
    S.Gt -> compareOpPrim ">"
    S.Le -> compareOpPrim "<="
    S.Ge -> compareOpPrim ">="
    S.And -> do
      lexp <- toExp x
      rexp <- toExp y
      match lexp [(Right $ Con "False" [], \_ -> Atom <$> def lexp), (Left ("lexp", cTypeOf lexp), \_ -> Atom <$> def rexp)]
    S.Or -> do
      lexp <- toExp x
      rexp <- toExp y
      match lexp [(Right $ Con "True" [], \_ -> Atom <$> def lexp), (Left ("lexp", cTypeOf lexp), \_ -> Atom <$> def rexp)]
  where
    arithOpPrim lcon rcon resultCon@(Con _ [t]) primName primType = do
      lexp <- toExp x
      rexp <- toExp y
      match
        lexp
        [ ( Right lcon,
            \[lval] ->
              match
                rexp
                [ ( Right rcon,
                    \[rval] ->
                      match
                        (PrimCall primName primType [Var lval, Var rval])
                        [ ( Left ("result", t),
                            \[result] ->
                              let_ "ret" (Pack resultCon [Var result]) (PackT [resultCon]) $ pure . Atom . Var
                          )
                        ]
                  )
                ]
          )
        ]
    arithOpPrim _ _ _ _ _ = bug Unreachable
    compareOpPrim primName = runDef $ do
      lval <- def =<< toExp x
      rval <- def =<< toExp y
      match
        (PrimCall primName (cTypeOf lval :-> cTypeOf rval :-> PackT [Con "True" [], Con "False" []]) [lval, rval])
        [(Left ("cmp", PackT [Con "True" [], Con "False" []]), \[result] -> pure $ Atom $ Var result)]
toExp (S.Match _ e cs) = do
  e' <- toExp e
  cs' <- traverse ?? cs $ \(p, v) -> crushPat p $ toExp v
  pure $ Match e' cs'

crushPat :: (MonadUniq m, MonadState (IdMap Type (Id CType)) m) => S.Pat (Id Type) -> m (Exp (Id CType)) -> m (Case (Id CType))
crushPat (S.VarP x) = \e -> do
  x' <- newId (cTypeOf $ typeOf x) (x ^. idName)
  modify $ at x ?~ x'
  Bind x' <$> e
crushPat (S.TupleP xs) = go xs []
  where
    go [] acc e =
      let acc' = reverse acc
       in Unpack (Con ("Tuple" <> T.pack (show $ length acc)) $ map cTypeOf acc') acc' <$> e
    go (p : ps) acc e = do
      x <- newId (cTypeOf $ typeOf p) "p"
      go ps (x : acc) $ do
        clause <- crushPat p e
        pure $ Match (Atom $ Var x) [clause]

let_ ::
  MonadUniq m =>
  String ->
  Obj (Id CType) ->
  CType ->
  (Id CType -> WriterT (Endo (Exp (Id CType))) m (Exp (Id CType))) ->
  m (Exp (Id CType))
let_ name o otype body = do
  v <- newId otype name
  body' <- runDef $ body v
  pure $ Let [(v, o)] body'

match :: MonadUniq f => Exp (Id CType) -> NonEmpty (Either (String, CType) Con, [Id CType] -> WriterT (Endo (Exp (Id CType))) f (Exp (Id CType))) -> f (Exp (Id CType))
match e ps = do
  cs' <- traverse ?? ps $ \case
    (Right con@(Con _ ts), body) -> do
      vs <- traverse (newId ?? "p") ts
      body' <- runDef $ body vs
      pure $ Unpack con vs body'
    (Left (name, ty), body) -> do
      x' <- newId ty name
      body' <- runDef $ body [x']
      pure $ Bind x' body'
  pure $ Match e cs'

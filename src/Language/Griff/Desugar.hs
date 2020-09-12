{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Desugar where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Griff.Extension
import Language.Griff.Grouping
import qualified Language.Griff.RnEnv as Rn
import Language.Griff.Syntax as G
import Language.Griff.TcEnv (TcEnv)
import qualified Language.Griff.TcEnv as Tc
import Language.Griff.Type as GT
import qualified Language.Griff.Typing as Typing
import Language.Malgo.IR.Core as C
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType as C
import qualified Text.PrettyPrint.HughesPJ as P
import Control.Exception (assert)

data DesugarEnv = DesugarEnv
  { _varEnv :: Map (Id ()) (Id CType),
    _tcEnv :: TcEnv
  }
  deriving stock (Show)

instance Semigroup DesugarEnv where
  DesugarEnv v1 t1 <> DesugarEnv v2 t2 = DesugarEnv (v1 <> v2) (t1 <> t2)

instance Monoid DesugarEnv where
  mempty = DesugarEnv mempty mempty

makeLenses ''DesugarEnv

desugar :: (MonadUniq m, MonadFail m, MonadIO m) => TcEnv -> BindGroup (Griff 'TypeCheck) -> m (C.Exp (Id CType))
desugar tcEnv ds = do
  dcEnv <- genDcEnv tcEnv
  runReaderT (dcBindGroup ds) dcEnv

genDcEnv :: (MonadIO m, MonadUniq m) => TcEnv -> m DesugarEnv
genDcEnv env = do
  -- let add_i32 = fromJust $ Map.lookup "add_i32#" (view (Tc.rnEnv . Rn.varEnv) tcEnv)
  -- let Forall _ add_i32_type = fromJust $ Map.lookup add_i32 (view Tc.varEnv tcEnv)
  -- add_i32' <- join $ newId <$> dcType add_i32_type <*> pure "add_i32#"

  let add_i64 = fromJust $ Map.lookup "add_i64#" (view (Tc.rnEnv . Rn.varEnv) env)
  let Forall _ add_i64_type = fromJust $ Map.lookup add_i64 (view Tc.varEnv env)
  add_i64' <- join $ newId <$> dcType add_i64_type <*> pure "add_i64#"

  pure $
    mempty & varEnv
      .~ Map.fromList
        [ -- (add_i32, add_i32'),
          (add_i64, add_i64')
        ]
        & tcEnv
      .~ env

dcBindGroup :: (MonadUniq m, MonadReader DesugarEnv m, MonadFail m, MonadIO m) => BindGroup (Griff 'TypeCheck) -> m (C.Exp (Id CType))
dcBindGroup bg = do
  (env, dataDefs') <- first mconcat <$> mapAndUnzipM dcDataDef (bg ^. dataDefs)
  local (env <>) $ do
    (env, forigns') <- first mconcat <$> mapAndUnzipM dcForign (bg ^. forigns)
    local (env <>) $ do
      scDefs' <- dcScDefGroup (bg ^. scDefs)
      pure $
        buildLet dataDefs' $
          buildLet (map (: []) forigns') $
            buildLet scDefs' $
              searchMain $ mconcat scDefs'
  where
    buildLet [] e = e
    buildLet (x : xs) e = Let x (buildLet xs e)
    searchMain ((bindId, Fun [] _) : _)
      | bindId ^. idName == "main" = Call (C.Var bindId) []
    searchMain (_ : xs) = searchMain xs
    searchMain _ = C.PrimCall "mainIsNotDefined" ([] :-> VarT (-1)) []

dcScDefGroup :: (MonadUniq f, MonadReader DesugarEnv f, MonadFail f, MonadIO f) => [[ScDef (Griff 'TypeCheck)]] -> f [[(Id CType, Obj (Id CType))]]
dcScDefGroup [] = pure []
dcScDefGroup (ds : dss) = do
  (env, ds') <- dcScDefs ds
  local (env <>) $ (ds' :) <$> dcScDefGroup dss

-- 相互再帰的なグループをdesugar
dcScDefs :: (MonadUniq f, MonadReader DesugarEnv f, MonadFail f, MonadIO f) => [ScDef (Griff 'TypeCheck)] -> f (DesugarEnv, [(Id CType, Obj (Id CType))])
dcScDefs ds = do
  env <- foldMapA ?? ds $ \(_, f, _, _) -> do
    Just (Forall _ fType) <- Map.lookup f <$> view (tcEnv . Tc.varEnv)
    f' <- join $ newId <$> dcType fType <*> pure (f ^. idName)
    pure $ mempty & varEnv .~ Map.singleton f f'
  local (env <>) $ (env,) <$> traverse dcScDef ds

dcScDef :: (MonadUniq f, MonadReader DesugarEnv f, MonadIO f, MonadFail f) => ScDef (Griff 'TypeCheck) -> f (Id CType, Obj (Id CType))
dcScDef (x, name, params, expr) = do
  (paramTypes, _) <- splitTyArr <$> Typing.zonkType (view typeOf x)
  params' <- traverse ?? zip params paramTypes $ \(pId, pType) ->
    join $ newId <$> dcType pType <*> pure (pId ^. idName)
  local (over varEnv (Map.fromList (zip params params') <>)) $ do
    expr' <- dcExp expr
    fun <- curryFun params' expr'
    name' <- lookupName name
    assert (cTypeOf name' == cTypeOf fun) $ pure ()
    pure (name', fun)

dcForign :: (MonadReader DesugarEnv f, MonadUniq f, MonadIO f) => Forign (Griff 'TypeCheck) -> f (DesugarEnv, (Id CType, Obj (Id CType)))
dcForign (x@(WithType (_, primName) _), name, _) = do
  name' <- join $ newId <$> dcType (x ^. typeOf) <*> pure (name ^. idName)
  (paramTypes, _) <- splitTyArr <$> Typing.zonkType (view typeOf x)
  params <- traverse ?? paramTypes $ \paramType -> do
    join $ newId <$> dcType paramType <*> pure "$p"
  primType <- dcType (view typeOf x)
  fun <- curryFun params $ C.PrimCall primName primType (map C.Var params)
  pure (mempty & varEnv .~ Map.singleton name name', (name', fun))

dcDataDef :: (MonadUniq m, MonadReader DesugarEnv m, MonadFail m, MonadIO m) => DataDef (Griff 'TypeCheck) -> m (DesugarEnv, [(Id CType, Obj (Id CType))])
dcDataDef (_, name, _, cons) = do
  fmap (first mconcat) $
    mapAndUnzipM ?? cons $ \(conName, _) -> do
      Just (GT.TyCon name') <- Map.lookup name <$> view (tcEnv . Tc.typeEnv)
      Just (_, conMap) <- Map.lookup name' <$> view (tcEnv . Tc.tyConEnv)
      typ <- dcType $ fromJust $ List.lookup conName conMap
      conName' <- newId typ (conName ^. idName)
      case typ of
        paramTypes :-> retType -> do
          ps <- traverse (\t -> newId t "$p") paramTypes
          unfoldedType <- unfoldType $ snd $ splitTyArr (fromJust $ List.lookup conName conMap)
          obj <-
            curryFun ps
              =<< runDef
                ( do
                    packed <- let_ unfoldedType $ Pack unfoldedType (C.Con (T.pack $ show $ pPrint conName) paramTypes) $ map C.Var ps
                    pure $ Cast retType packed
                )
          pure (mempty & varEnv .~ Map.singleton conName conName', (conName', obj))
        _ -> do
          unfoldedType <- unfoldType $ fromJust $ List.lookup conName conMap
          obj <- fmap (Fun []) $
            runDef $ do
              packed <- let_ unfoldedType $ Pack unfoldedType (C.Con (T.pack $ show $ pPrint conName) []) []
              pure $ Cast typ packed
          pure (mempty & varEnv .~ Map.singleton conName conName', (conName', obj))

dcExp :: (HasCallStack, MonadUniq m, MonadReader DesugarEnv m, MonadIO m, MonadFail m) => G.Exp (Griff 'TypeCheck) -> m (C.Exp (Id CType))
dcExp (G.Var x name) = do
  name' <- lookupName name
  typ <- Typing.zonkType (view typeOf x)
  case (typ, cTypeOf name') of
    (GT.TyLazy {}, [] :-> _) -> pure $ Atom $ C.Var name'
    (GT.TyLazy {}, _) -> errorDoc $ "Invalid TyLazy:" <+> P.quotes (pPrint $ cTypeOf name')
    (_, [] :-> _) -> pure $ Call (C.Var name') []
    _ -> pure $ Atom $ C.Var name'
dcExp (G.Con _ name) = do
  name' <- lookupName name
  case cTypeOf name' of
    [] :-> _ -> pure $ Call (C.Var name') []
    _ -> pure $ Atom $ C.Var name'
dcExp (G.Unboxed _ u) = pure $
  Atom $
    C.Unboxed $ case u of
      G.Int32 _ -> error "Int32# is not implemented"
      G.Int64 x -> C.Int $ toInteger x
      G.Float _ -> error "Float# is not implemented"
      G.Double x -> C.Float x
      G.Char x -> C.Char x
      G.String x -> C.String x
dcExp (G.Apply _ f x) = runDef $ do
  f' <- bind =<< dcExp f
  x' <- bind =<< dcExp x
  pure $ Call f' [x']
dcExp (G.OpApp _ op x y) = runDef $ do
  op' <- lookupName op
  x' <- bind =<< dcExp x
  y' <- bind =<< dcExp y
  e1 <- bind (Call (C.Var op') [x'])
  pure $ Call e1 [y']
dcExp (G.Fn _ cs) = do -- TODO: lazy valueの変換を追加
  (funcBuilder, ps) <- genFuncBuilder cs
  cases <- genCases cs
  funcBuilder $
    runDef $ do
      case ps of
        [(ty, p)] -> do
          p' <- join $ cast <$> unfoldType ty <*> pure (Atom p)
          pure $ Match (Atom p') cases
        ps -> do
          let con = C.Con ("Tuple" <> T.pack (show $ length ps)) $ map (cTypeOf . snd) ps
          let ty = SumT $ Set.singleton con
          args <- let_ ty (Pack ty con $ map snd ps)
          pure $ Match (Atom args) cases
dcExp (G.Tuple _ es) = runDef $ do
  es' <- traverse (bind <=< dcExp) es
  let con = C.Con ("Tuple" <> T.pack (show $ length es)) $ map cTypeOf es'
  let ty = SumT $ Set.singleton con
  tuple <- let_ ty $ Pack ty con es'
  pure $ Atom tuple
dcExp (G.Force _ e) = runDef $ do
  e' <- bind =<< dcExp e
  pure $ Call e' []

genFuncBuilder ::
  (MonadIO m, MonadUniq m) =>
  [Clause (Griff 'TypeCheck)] ->
  m
    ( m (C.Exp (Id CType)) -> m (C.Exp (Id CType)),
      [(GT.Type, Atom (Id CType))]
    )
genFuncBuilder [] = bug Unreachable
genFuncBuilder (c : _) = do
  (paramTypes, _) <- splitTyArr <$> Typing.zonkType (c ^. typeOf)
  paramTypes' <- traverse dcType paramTypes
  ps <- traverse (\p -> newId p "$p") paramTypes'
  let funcBuilder genBody = do
        obj <- curryFun ps =<< genBody
        typ <- dcType =<< Typing.zonkType (view typeOf c)
        runDef $ Atom <$> let_ typ obj
  pure (funcBuilder, zip paramTypes $ map C.Var ps)

genCases :: (MonadUniq m, MonadIO m, MonadReader DesugarEnv m, MonadFail m) => [Clause (Griff 'TypeCheck)] -> m (NonEmpty (Case (Id CType)))
genCases [] = error "Empty cases"
genCases (c : cs) = do
  c' <- c & \(Clause _ ps e) -> crushPat ps (dcExp e)
  cs' <- traverse (\(Clause _ ps e) -> crushPat ps (dcExp e)) cs
  pure (c' :| cs')

crushPat :: (MonadUniq m, MonadIO m, MonadReader DesugarEnv m, MonadFail m) => [Pat (Griff 'TypeCheck)] -> m (C.Exp (Id CType)) -> m (Case (Id CType))
crushPat (x : xs@(_ : _)) = go (x : xs) []
  where
    go [] acc e = do
      acc <- pure $ reverse acc
      Unpack (C.Con ("Tuple" <> T.pack (show $ length acc)) $ map cTypeOf acc) acc <$> e
    go (p : ps) acc e = do
      x <- join $ newId <$> dcType (p ^. typeOf) <*> pure "$p"
      go ps (x : acc) $ do
        clause <- crushPat [p] e
        expr <- runDef $ Atom <$> (join $ cast <$> unfoldType (p ^. typeOf) <*> pure (Atom $ C.Var x))
        pure $ Match expr (clause :| [])
crushPat [VarP x v] = \e -> do
  v' <- join $ newId <$> dcType (x ^. typeOf) <*> pure (v ^. idName)
  local (varEnv %~ Map.insert v v') $ Bind v' <$> e
crushPat [ConP _ con ps] = go ps []
  where
    go [] acc e = do
      acc <- pure $ reverse acc
      paramTypes <- traverse (dcType . view typeOf) ps
      let con' = C.Con (T.pack $ show $ pPrint con) paramTypes
      Unpack con' acc <$> e
    go (p : ps) acc e = do
      x <- join $ newId <$> dcType (p ^. typeOf) <*> pure "$p"
      go ps (x : acc) $ do
        clause <- crushPat [p] e
        expr <- runDef $ Atom <$> (join $ cast <$> unfoldType (p ^. typeOf) <*> pure (Atom $ C.Var x))
        pure $ Match expr (clause :| [])

dcType :: (HasCallStack, MonadIO m) => GT.Type -> m CType
dcType (GT.TyApp t1 t2) = do
  let (con, ts) = splitCon t1 t2
  DataT (T.pack $ show $ pPrint con) <$> traverse dcType ts
dcType (GT.TyVar i) = pure $ VarT $ i ^. idUniq
dcType (GT.TyCon con) | kind con == Star = pure $ DataT (T.pack $ show $ pPrint con) []
dcType (GT.TyPrim GT.Int32T) = error "Int32# is not implemented"
dcType (GT.TyPrim GT.Int64T) = pure C.IntT
dcType (GT.TyPrim GT.FloatT) = error "Float# is not implemented"
dcType (GT.TyPrim GT.DoubleT) = pure C.FloatT
dcType (GT.TyPrim GT.CharT) = pure C.CharT
dcType (GT.TyPrim GT.StringT) = pure C.StringT
dcType (GT.TyArr t1 t2) = do
  t1' <- dcType t1
  t2' <- dcType t2
  pure $ [t1'] :-> t2'
dcType (GT.TyTuple ts) =
  SumT . Set.singleton . C.Con ("Tuple" <> T.pack (show $ length ts)) <$> traverse dcType ts
dcType (GT.TyLazy t) = ([] :->) <$> dcType t
dcType (GT.TyMeta tv) = do
  mtype <- Typing.readMetaTv tv
  case mtype of
    Just t -> dcType t
    Nothing -> error "TyMeta must be removed"

dcXType :: (MonadReader DesugarEnv m, MonadIO m) => G.Type (Griff 'TypeCheck) -> m CType
dcXType t =
  view tcEnv
    >>= runReaderT (Typing.transType t)
    >>= dcType

unfoldType :: (MonadReader DesugarEnv m, MonadFail m, MonadIO m) => GT.Type -> m CType
unfoldType (GT.TyApp t1 t2) = do
  let (con, ts) = splitCon t1 t2
  Just (as, conMap) <- Map.lookup con <$> view (tcEnv . Tc.tyConEnv)
  let conMap' = over (mapped . _2) (Typing.applySubst $ Map.fromList $ zip as ts) conMap
  SumT
    . Set.fromList
    <$> traverse
      ( \(conName, conType) ->
          C.Con (T.pack $ show $ pPrint conName) <$> traverse dcType (fst $ splitTyArr conType)
      )
      conMap'
unfoldType (GT.TyCon con) | kind con == Star = do
  Just ([], conMap) <- Map.lookup con <$> view (tcEnv . Tc.tyConEnv)
  SumT
    . Set.fromList
    <$> traverse
      ( \(conName, conType) ->
          C.Con (T.pack $ show $ pPrint conName) <$> traverse dcType (fst $ splitTyArr conType)
      )
      conMap
unfoldType t = dcType t

-- Desugar Monad

lookupName :: (HasCallStack, MonadReader DesugarEnv m) => Id () -> m (Id CType)
lookupName name = do
  env <- view varEnv
  case Map.lookup name env of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> P.quotes (pPrint name)

lookupType :: (MonadReader DesugarEnv m, MonadFail m, MonadIO m) => Id () -> m CType
lookupType name = do
  env <- view (tcEnv . Tc.typeEnv)
  case Map.lookup name env of
    Just typ -> dcType typ
    Nothing -> bug Unreachable

runDef :: Functor f => WriterT (Endo a) f a -> f a
runDef m = uncurry (flip appEndo) <$> runWriterT m

let_ ::
  (MonadUniq m, MonadWriter (Endo (C.Exp (Id a))) m) =>
  a ->
  Obj (Id a) ->
  m (Atom (Id a))
let_ otype obj = do
  x <- newId otype "$let"
  tell $ Endo $ \e -> Let [(x, obj)] e
  pure (C.Var x)

destruct ::
  (MonadUniq m, MonadWriter (Endo (C.Exp (Id CType))) m) =>
  C.Exp (Id CType) ->
  Con ->
  m [Atom (Id CType)]
destruct val con@(C.Con _ ts) = do
  vs <- traverse (newId ?? "$p") ts
  tell $ Endo $ \e -> Match val (Unpack con vs e :| [])
  pure $ map C.Var vs

bind :: (MonadUniq m, MonadWriter (Endo (C.Exp (Id CType))) m) => C.Exp (Id CType) -> m (Atom (Id CType))
bind (Atom a) = pure a
bind v = do
  x <- newId (cTypeOf v) "$d"
  tell $ Endo $ \e -> Match v (Bind x e :| [])
  pure (C.Var x)

cast ::
  (MonadUniq f, MonadWriter (Endo (C.Exp (Id CType))) f) =>
  CType ->
  C.Exp (Id CType) ->
  f (Atom (Id CType))
cast ty e
  | ty == cTypeOf e = bind e
  | otherwise = do
    v <- bind e
    x <- newId ty "$cast"
    tell $ Endo $ \e -> Match (Cast ty v) (Bind x e :| [])
    pure (C.Var x)

curryFun :: MonadUniq m => [Id CType] -> C.Exp (Id CType) -> m (Obj (Id CType))
curryFun [] (Let [(v1, fun)] (Atom (C.Var v2))) | v1 == v2 = pure fun
curryFun [] e = errorDoc $ "Invalid expression:" <+> P.quotes (pPrint e)
curryFun [x] e = pure $ Fun [x] e
curryFun ps@(_ : _) e = do
  curryFun' ps []
  where
    curryFun' [x] as = do
      x' <- newId (cTypeOf x) (x ^. idName)
      body <- runDef $ do
        fun <- let_ (cTypeOf $ Fun ps e) (Fun ps e)
        pure $ C.Call fun $ reverse (C.Var x' : as)
      pure $ Fun [x'] body
    curryFun' (x : xs) as = do
      x' <- newId (cTypeOf x) (x ^. idName)
      funObj <- curryFun' xs (C.Var x' : as)
      body <- runDef $ do
        fun <- let_ (cTypeOf funObj) funObj
        pure $ Atom fun
      pure $ Fun [x'] body

-- splitFoo

splitCon :: GT.Type -> GT.Type -> (Id Kind, [GT.Type])
splitCon (GT.TyCon con) t = (con, [t])
splitCon (GT.TyApp t1 t2) t3 =
  let (dataCon, ts) = splitCon t1 t2
   in (dataCon, ts <> [t3])
splitCon _ _ = bug Unreachable

splitTyArr :: GT.Type -> ([GT.Type], GT.Type)
splitTyArr (GT.TyArr t1 t2) =
  let (ps, r) = splitTyArr t2
   in (t1 : ps, r)
splitTyArr t = ([], t)
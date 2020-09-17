{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Desugar where

import Control.Exception (assert)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
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
import Language.Malgo.IR.Op
import Language.Malgo.Id
import Language.Malgo.Monad
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType as C
import qualified Text.PrettyPrint.HughesPJ as P

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
  (dcEnv, prims) <- genPrimitive tcEnv
  Let prims <$> runReaderT (dcBindGroup ds) dcEnv

genPrimitive :: (MonadUniq m, MonadIO m, MonadFail m) => TcEnv -> m (DesugarEnv, [(Id CType, Obj (Id CType))])
genPrimitive env =
  do
    let add_i64 = fromJust $ Map.lookup "add_i64#" (view (Tc.rnEnv . Rn.varEnv) env)
    let Forall _ add_i64_type = fromJust $ Map.lookup add_i64 (view Tc.varEnv env)
    add_i64' <- join $ newId <$> dcType add_i64_type <*> pure "add_i64#"
    add_i64_param <- newId (SumT $ Set.singleton (C.Con "Tuple2" [IntT, IntT])) "$p"
    add_i64_fun <- fmap (Fun [add_i64_param]) $
      runDef $ do
        [x, y] <- destruct (Atom $ C.Var add_i64_param) (C.Con "Tuple2" [IntT, IntT])
        pure $ BinOp Add x y

    let newEnv =
          mempty & varEnv
            .~ Map.fromList
              [(add_i64, add_i64')]
              & tcEnv
            .~ env

    pure (newEnv, [(add_i64', add_i64_fun)])

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
dcScDef (WithType pos _, name, [], expr) = do
  typ <- Typing.zonkType $ expr ^. toType
  case typ of
    GT.TyArr {} -> dc
    GT.TyLazy {} -> dc
    _ -> errorOn pos $ "Invalid Toplevel Declaration:" <+> P.quotes (pPrint name <+> ":" <+> pPrint typ)
  where
    dc = do
      name' <- lookupName name
      expr' <- dcExp expr
      fun <- curryFun [] expr'
      pure (name', fun)
dcScDef (x, name, params, expr) = do
  (paramTypes, _) <- splitTyArr <$> Typing.zonkType (view toType x)
  params' <- traverse ?? zip params paramTypes $ \(pId, pType) ->
    join $ newId <$> dcType pType <*> pure (pId ^. idName)
  local (over varEnv (Map.fromList (zip params params') <>)) $ do
    expr' <- dcExp expr
    fun <- curryFun params' expr'
    name' <- lookupName name
    pure (name', fun)

dcForign :: (MonadReader DesugarEnv f, MonadUniq f, MonadIO f) => Forign (Griff 'TypeCheck) -> f (DesugarEnv, (Id CType, Obj (Id CType)))
dcForign (x@(WithType (_, primName) _), name, _) = do
  name' <- join $ newId <$> dcType (x ^. toType) <*> pure (name ^. idName)
  (paramTypes, _) <- splitTyArr <$> Typing.zonkType (view toType x)
  params <- traverse ?? paramTypes $ \paramType -> do
    join $ newId <$> dcType paramType <*> pure "$p"
  primType <- dcType (view toType x)
  fun <- curryFun params $ C.PrimCall primName primType (map C.Var params)
  pure (mempty & varEnv .~ Map.singleton name name', (name', fun))

dcDataDef :: (MonadUniq m, MonadReader DesugarEnv m, MonadFail m, MonadIO m) => DataDef (Griff 'TypeCheck) -> m (DesugarEnv, [(Id CType, Obj (Id CType))])
dcDataDef (_, name, _, cons) = do
  fmap (first mconcat) $
    mapAndUnzipM ?? cons $ \(conName, _) -> do
      Just (GT.TyCon name') <- Map.lookup name <$> view (tcEnv . Tc.typeEnv)
      Just (_, conMap) <- Map.lookup name' <$> view (tcEnv . Tc.tyConEnv)
      let (paramTypes, retType) = splitTyArr $ fromJust $ List.lookup conName conMap
      paramTypes' <- traverse dcType paramTypes
      retType' <- dcType retType
      case paramTypes' of
        [] -> do
          conName' <- newId ([] :-> retType') (conName ^. idName)
          unfoldedType <- unfoldType $ fromJust $ List.lookup conName conMap
          obj <- fmap (Fun []) $
            runDef $ do
              packed <- let_ unfoldedType $ Pack unfoldedType (C.Con (T.pack $ show $ pPrint conName) []) []
              pure $ Cast retType' packed
          pure (mempty & varEnv .~ Map.singleton conName conName', (conName', obj))
        _ -> do
          typ <- dcType $ fromJust $ List.lookup conName conMap
          conName' <- newId typ (conName ^. idName)
          ps <- traverse (\t -> newId t "$p") paramTypes'
          unfoldedType <- unfoldType $ snd $ splitTyArr (fromJust $ List.lookup conName conMap)
          obj <-
            curryFun ps
              =<< runDef
                ( do
                    packed <- let_ unfoldedType $ Pack unfoldedType (C.Con (T.pack $ show $ pPrint conName) paramTypes') $ map C.Var ps
                    pure $ Cast retType' packed
                )
          pure (mempty & varEnv .~ Map.singleton conName conName', (conName', obj))

dcExp :: (HasCallStack, MonadUniq m, MonadReader DesugarEnv m, MonadIO m, MonadFail m) => G.Exp (Griff 'TypeCheck) -> m (C.Exp (Id CType))
dcExp (G.Var x name) = do
  name' <- lookupName name
  typ <- Typing.zonkType (view toType x)
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
  case cTypeOf f' of
    [xType] :-> _ -> do
      -- Note: [Cast Argument Type]
      --   x の型と f の引数の型は必ずしも一致しない
      --   適切な型にcastする必要がある
      x' <- cast xType =<< dcExp x
      pure $ Call f' [x']
    _ -> bug Unreachable
dcExp (G.OpApp _ op x y) = runDef $ do
  op' <- lookupName op
  case cTypeOf op' of
    [xType] :-> ([yType] :-> _) -> do
      -- Ref: [Cast Argument Type]
      x' <- cast xType =<< dcExp x
      y' <- cast yType =<< dcExp y
      e1 <- bind (Call (C.Var op') [x'])
      pure $ Call e1 [y']
dcExp (G.Fn x (Clause _ [] e : _)) = runDef $ do
  e' <- dcExp e
  typ <- dcType (x ^. toType)
  Atom <$> let_ typ (Fun [] e')
dcExp (G.Fn _ cs@(Clause _ ps e : _)) = do
  ps' <- traverse (\p -> join $ newId <$> dcType (p ^. toType) <*> pure "$p") ps
  typ <- dcType (e ^. toType)
  (pss, es) <- fmap (first List.transpose) $ mapAndUnzipM (\(Clause _ ps e) -> pure (ps, dcExp e)) cs
  body <- match ps' pss es (Error typ)
  obj <- curryFun ps' body
  runDef $ Atom <$> let_ (cTypeOf obj) obj
dcExp (G.Tuple _ es) = runDef $ do
  es' <- traverse (bind <=< dcExp) es
  let con = C.Con ("Tuple" <> T.pack (show $ length es)) $ map cTypeOf es'
  let ty = SumT $ Set.singleton con
  tuple <- let_ ty $ Pack ty con es'
  pure $ Atom tuple
dcExp (G.Force _ e) = runDef $ do
  e' <- bind =<< dcExp e
  pure $ Call e' []

-- TODO: The Implementation of Functional Programming Languages
-- を元にコメントを追加
match :: HasCallStack => (MonadReader DesugarEnv m, MonadFail m, MonadIO m, MonadUniq m) => [Id CType] -> [[Pat (Griff 'TypeCheck)]] -> [m (C.Exp (Id CType))] -> C.Exp (Id CType) -> m (C.Exp (Id CType))
match (u : us) (ps : pss) es err
  -- Variable Rule
  | all (\case VarP {} -> True; _ -> False) ps =
    {- Note: How to implement the Variable Rule?
        There are two (old) implementations.
        I believe that the Original impl is correct.
        But I'm not sure if this is correct.
        So, the `assert` is inserted in code.
        If I'm wrong, this `assert` is going to fail one day.
    -}
    -- -- Cast version
    -- match us pss (zipWith (\(VarP x v) e -> runDef $ do
    --   ty' <- dcType =<< Typing.zonkType (x ^. toType)
    --   C.Var u' <- cast ty' (Atom $ C.Var u)
    --   local (over varEnv (Map.insert v u')) $ lift e) ps es) err
    -- -- Original
    -- match us pss (zipWith (\(VarP _ v) e -> do
    --   local (over varEnv (Map.insert v u)) e) ps es) err
    -- -- Check Type version
    match
      us
      pss
      ( zipWith
          ( \(VarP x v) e -> do
              patTy <- dcType =<< Typing.zonkType (x ^. toType)
              -- if this assert fail, there are some bug about polymorphic type
              -- Ref: How to implement the Variable Rule?
              assert (patTy == cTypeOf u) $ pure ()
              local (over varEnv (Map.insert v u)) e
          )
          ps
          es
      )
      err
  -- Constructor Rule
  | all (\case ConP {} -> True; _ -> False) ps = do
    patType <- Typing.zonkType (head ps ^. toType)
    -- 型からコンストラクタの集合を求める
    cs <- constructors patType
    -- 各コンストラクタごとにC.Caseを生成する関数を生成する
    cases <- traverse genCase cs
    unfoldedType <- unfoldType patType
    pure $ Match (Cast unfoldedType $ C.Var u) $ NonEmpty.fromList cases
  -- The Mixture Rule
  | otherwise =
    match (u : us) (List.transpose [head $ List.transpose (ps : pss)]) [head es]
      =<< match (u : us) (List.transpose $ tail $ List.transpose (ps : pss)) (tail es) err
  where
    constructors (GT.TyApp t1 t2) = do
      let (con, ts) = splitCon t1 t2
      Just (as, conMap) <- view (tcEnv . Tc.tyConEnv . at con)
      let conMap' = over (mapped . _2) (Typing.applySubst $ Map.fromList $ zip as ts) conMap
      traverse ?? conMap' $ \(conName, conType) -> do
        paramTypes <- traverse dcType (fst $ splitTyArr conType)
        let ccon = C.Con (T.pack $ show $ pPrint conName) paramTypes
        params <- traverse (newId ?? "$p") paramTypes
        pure (conName, ccon, params)
    constructors (GT.TyCon con) | kind con == Star = do
      Just ([], conMap) <- view (tcEnv . Tc.tyConEnv . at con)
      traverse ?? conMap $ \(conName, conType) -> do
        paramTypes <- traverse dcType (fst $ splitTyArr conType)
        let ccon = C.Con (T.pack $ show $ pPrint conName) paramTypes
        params <- traverse (newId ?? "$p") paramTypes
        pure (conName, ccon, params)
    constructors t = errorDoc $ "Not valid type: " <+> pPrint t
    genCase (gcon, ccon, params) = do
      let (pss', es') = unzip $ group gcon (List.transpose (ps : pss)) es
      Unpack ccon params <$> match (params <> us) (List.transpose pss') es' err
    group gcon pss' es = mapMaybe (aux gcon) (zip pss' es)
    aux gcon (ConP _ gcon' ps : pss, e)
      | gcon == gcon' = Just (ps <> pss, e)
      | otherwise = Nothing
    aux _ (p : _, _) = errorDoc $ "Invalid pattern:" <+> pPrint p
match [] [] (e : _) _ = e
match _ [] [] err = pure err
match _ _ _ _ = bug Unreachable

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

curryFun :: (HasCallStack, MonadUniq m) => [Id CType] -> C.Exp (Id CType) -> m (Obj (Id CType))
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
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
    let add_i64 = fromJust $ view (Tc.rnEnv % Rn.varEnv % at "add_i64#") env
    let Forall _ add_i64_type = fromJust $ Map.lookup add_i64 (view Tc.varEnv env)
    add_i64' <- join $ newId <$> dcType add_i64_type <*> pure "add_i64#"
    add_i64_param <- newId (SumT $ Set.singleton (C.Con "Tuple2" [C.Int64T, C.Int64T])) "$p"
    add_i64_fun <- fmap (Fun [add_i64_param]) $
      runDef $ do
        [x, y] <- destruct (Atom $ C.Var add_i64_param) (C.Con "Tuple2" [C.Int64T, C.Int64T])
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
        buildLet (mconcat dataDefs') $
          buildLet forigns' $
            buildLet scDefs' $
              searchMain $ mconcat scDefs'
  where
    buildLet [] e = e
    buildLet (x : xs) e = Let x (buildLet xs e)
    searchMain ((bindId, Fun [] _) : _)
      | bindId ^. idName == "main" = Call (C.Var bindId) []
    searchMain (_ : xs) = searchMain xs
    searchMain _ = C.PrimCall "mainIsNotDefined" ([] :-> AnyT) []

dcScDefGroup :: (MonadUniq f, MonadReader DesugarEnv f, MonadFail f, MonadIO f) => [[ScDef (Griff 'TypeCheck)]] -> f [[(Id CType, Obj (Id CType))]]
dcScDefGroup [] = pure []
dcScDefGroup (ds : dss) = do
  (env, ds') <- dcScDefs ds
  local (env <>) $ (ds' :) <$> dcScDefGroup dss

-- 相互再帰的なグループをdesugar
dcScDefs :: (MonadUniq f, MonadReader DesugarEnv f, MonadFail f, MonadIO f) => [ScDef (Griff 'TypeCheck)] -> f (DesugarEnv, [(Id CType, Obj (Id CType))])
dcScDefs ds = do
  env <- foldMapA ?? ds $ \(_, f, _, _) -> do
    Just (Forall _ fType) <- asks $ view (tcEnv % Tc.varEnv % at f)
    f' <- join $ newId <$> dcType fType <*> pure (f ^. idName)
    pure $ mempty & varEnv .~ Map.singleton f f'
  local (env <>) $ (env,) <$> foldMapA dcScDef ds

dcScDef :: (MonadUniq f, MonadReader DesugarEnv f, MonadIO f, MonadFail f) => ScDef (Griff 'TypeCheck) -> f [(Id CType, Obj (Id CType))]
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
      (fun, inner) <- curryFun [] expr'
      pure ((name', fun) : inner)
dcScDef (x, name, params, expr) = do
  (paramTypes, _) <- splitTyArr <$> Typing.zonkType (view toType x)
  params' <- traverse ?? zip params paramTypes $ \(pId, pType) ->
    join $ newId <$> dcType pType <*> pure (pId ^. idName)
  local (over varEnv (Map.fromList (zip params params') <>)) $ do
    expr' <- dcExp expr
    (fun, inner) <- curryFun params' expr'
    name' <- lookupName name
    pure ((name', fun) : inner)

dcForign :: (MonadReader DesugarEnv f, MonadUniq f, MonadIO f) => Forign (Griff 'TypeCheck) -> f (DesugarEnv, [(Id CType, Obj (Id CType))])
dcForign (x@(WithType (_, primName) _), name, _) = do
  name' <- join $ newId <$> dcType (x ^. toType) <*> pure (name ^. idName)
  (paramTypes, _) <- splitTyArr <$> Typing.zonkType (view toType x)
  params <- traverse ?? paramTypes $ \paramType -> do
    join $ newId <$> dcType paramType <*> pure "$p"
  primType <- dcType (view toType x)
  (fun, inner) <- curryFun params $ C.PrimCall primName primType (map C.Var params)
  pure (mempty & varEnv .~ Map.singleton name name', ((name', fun) : inner))

dcDataDef :: (MonadUniq m, MonadReader DesugarEnv m, MonadFail m, MonadIO m) => DataDef (Griff 'TypeCheck) -> m (DesugarEnv, [[(Id CType, Obj (Id CType))]])
dcDataDef (_, name, _, cons) = do
  fmap (first mconcat) $
    mapAndUnzipM ?? cons $ \(conName, _) -> do
      Just (GT.TyCon name') <- asks $ view (tcEnv % Tc.typeEnv % at name)
      Just (_, conMap) <- asks $ view (tcEnv % Tc.tyConEnv % at name')
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
          pure (mempty & varEnv .~ Map.singleton conName conName', [(conName', obj)])
        _ -> do
          typ <- dcType $ fromJust $ List.lookup conName conMap
          conName' <- newId typ (conName ^. idName)
          ps <- traverse (\t -> newId t "$p") paramTypes'
          unfoldedType <- unfoldType $ snd $ splitTyArr (fromJust $ List.lookup conName conMap)
          (obj, inner) <- do
            curryFun ps
              =<< runDef
                ( do
                    packed <- let_ unfoldedType $ Pack unfoldedType (C.Con (T.pack $ show $ pPrint conName) paramTypes') $ map C.Var ps
                    pure $ Cast retType' packed
                )
          pure (mempty & varEnv .~ Map.singleton conName conName', ((conName', obj) : inner))

dcUnboxed :: G.Unboxed -> C.Unboxed
dcUnboxed (G.Int32 _) = error "Int32# is not implemented"
dcUnboxed (G.Int64 x) = C.Int64 $ toInteger x
dcUnboxed (G.Float _) = error "Float# is not implemented"
dcUnboxed (G.Double x) = C.Double x
dcUnboxed (G.Char x) = C.Char x
dcUnboxed (G.String x) = C.String x

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
dcExp (G.Unboxed _ u) = pure $ Atom $ C.Unboxed $ dcUnboxed u
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
    _ -> bug Unreachable
dcExp (G.Fn x (Clause _ [] e : _)) = runDef $ do
  e' <- dcExp e
  typ <- dcType (x ^. toType)
  Atom <$> let_ typ (Fun [] e')
dcExp (G.Fn _ cs@(Clause _ ps e : _)) = do
  ps' <- traverse (\p -> join $ newId <$> dcType (p ^. toType) <*> pure "$p") ps
  typ <- dcType (e ^. toType)
  (pss, es) <- fmap (first List.transpose) $ mapAndUnzipM (\(Clause _ ps e) -> pure (ps, dcExp e)) cs
  body <- match ps' pss es (Error typ)
  (obj, inner) <- curryFun ps' body
  v <- newId (cTypeOf obj) "$fun"
  pure $ Let ((v, obj) : inner) $ Atom $ C.Var v
dcExp (G.Fn _ []) = bug Unreachable
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
          ( \case
              VarP x v -> \e -> do
                patTy <- dcType =<< Typing.zonkType (x ^. toType)
                -- if this assert fail, there are some bug about polymorphic type
                -- Ref: How to implement the Variable Rule?
                assert (patTy == cTypeOf u) $ pure ()
                local (over varEnv (Map.insert v u)) e
              _ -> bug Unreachable
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
  | all (\case UnboxedP {} -> True; _ -> False) ps = do
    let cs =
          map
            ( \case
                UnboxedP _ x -> dcUnboxed x
                _ -> bug Unreachable
            )
            ps
    cases <- traverse ?? cs $ \c -> Switch c <$> match us pss es err
    hole <- newId (cTypeOf u) "$_"
    pure $ Match (Atom $ C.Var u) $ NonEmpty.fromList (cases <> [C.Bind hole err])
  -- The Mixture Rule
  | otherwise =
    match (u : us) (List.transpose [head $ List.transpose (ps : pss)]) [head es]
      =<< match (u : us) (List.transpose $ tail $ List.transpose (ps : pss)) (tail es) err
  where
    constructors t
      | case t of GT.TyApp {} -> False; GT.TyCon {} -> False; _ -> True = errorDoc $ "Not valid type: " <+> pPrint t
      | otherwise = do
        let (con, ts) = splitCon t
        Just (as, conMap) <- asks $ view (tcEnv % Tc.tyConEnv % at con)
        let conMap' = over (mapped % _2) (Typing.applySubst $ Map.fromList $ zip as ts) conMap
        traverse (uncurry buildConInfo) conMap'
    buildConInfo conName conType = do
      paramTypes <- traverse dcType (fst $ splitTyArr conType)
      let ccon = C.Con (T.pack $ show $ pPrint conName) paramTypes
      params <- traverse (newId ?? "$p") paramTypes
      pure (conName, ccon, params)
    genCase (gcon, ccon, params) = do
      let (pss', es') = unzip $ group gcon (List.transpose (ps : pss)) es
      Unpack ccon params <$> match (params <> us) (List.transpose pss') es' err
    group gcon pss' es = mapMaybe (aux gcon) (zip pss' es)
    aux gcon (ConP _ gcon' ps : pss, e)
      | gcon == gcon' = Just (ps <> pss, e)
      | otherwise = Nothing
    aux _ (p : _, _) = errorDoc $ "Invalid pattern:" <+> pPrint p
    aux _ ([], _) = bug Unreachable
match [] [] (e : _) _ = e
match _ [] [] err = pure err
match _ _ _ _ = bug Unreachable

dcType :: (HasCallStack, MonadIO m) => GT.Type -> m CType
dcType t@GT.TyApp {} = do
  let (con, ts) = splitCon t
  DataT (T.pack $ show $ pPrint con) <$> traverse dcType ts
dcType (GT.TyVar _) = pure AnyT
dcType (GT.TyCon con)
  | kind con == Star = pure $ DataT (T.pack $ show $ pPrint con) []
  | otherwise = errorDoc $ "Invalid kind:" <+> pPrint con <+> ":" <+> pPrint (kind con)
dcType (GT.TyPrim GT.Int32T) = error "Int32# is not implemented"
dcType (GT.TyPrim GT.Int64T) = pure C.Int64T
dcType (GT.TyPrim GT.FloatT) = error "Float# is not implemented"
dcType (GT.TyPrim GT.DoubleT) = pure C.DoubleT
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
  asks (view tcEnv)
    >>= runReaderT (Typing.transType t)
    >>= dcType

unfoldType :: (MonadReader DesugarEnv m, MonadFail m, MonadIO m) => GT.Type -> m CType
unfoldType t@GT.TyApp {} = do
  let (con, ts) = splitCon t
  Just (as, conMap) <- asks $ view (tcEnv % Tc.tyConEnv % at con)
  let conMap' = over (mapped % _2) (Typing.applySubst $ Map.fromList $ zip as ts) conMap
  SumT
    . Set.fromList
    <$> traverse
      ( \(conName, conType) ->
          C.Con (T.pack $ show $ pPrint conName) <$> traverse dcType (fst $ splitTyArr conType)
      )
      conMap'
unfoldType (GT.TyCon con) | kind con == Star = do
  Just ([], conMap) <- asks $ view (tcEnv % Tc.tyConEnv % at con)
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
  mname' <- asks $ view (varEnv % at name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> P.quotes (pPrint name)

lookupType :: (MonadReader DesugarEnv m, MonadFail m, MonadIO m) => Id () -> m CType
lookupType name = do
  mtyp <- asks $ view (tcEnv % Tc.typeEnv % at name)
  case mtyp of
    Just typ -> dcType typ
    Nothing -> bug Unreachable

curryFun :: (HasCallStack, MonadUniq m) => [Id CType] -> C.Exp (Id CType) -> m (Obj (Id CType), [(Id CType, Obj (Id CType))])
curryFun [] e@(Let ds (Atom (C.Var v))) = case List.lookup v ds of
  Just fun -> pure (fun, filter ((/= v) . fst) ds)
  Nothing -> errorDoc $ "Invalid expression:" <+> P.quotes (pPrint e)
curryFun [] e = errorDoc $ "Invalid expression:" <+> P.quotes (pPrint e)
curryFun [x] e = pure (Fun [x] e, [])
curryFun ps@(_ : _) e =
  curryFun' ps []
  where
    curryFun' [] _ = bug Unreachable
    curryFun' [x] as = do
      x' <- newId (cTypeOf x) (x ^. idName)
      fun <- newId (cTypeOf $ Fun ps e) "$curry"
      let body = C.Call (C.Var fun) $ reverse $ C.Var x' : as
      pure (Fun [x'] body, [(fun, Fun ps e)])
    curryFun' (x : xs) as = do
      x' <- newId (cTypeOf x) (x ^. idName)
      (funObj, inner) <- curryFun' xs (C.Var x' : as)
      body <- runDef $ do
        fun <- let_ (cTypeOf funObj) funObj
        pure $ Atom fun
      pure (Fun [x'] body, inner)
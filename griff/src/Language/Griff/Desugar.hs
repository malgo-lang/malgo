{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | GriffをKoriel.Coreに変換（脱糖衣）する
module Language.Griff.Desugar (desugar) where

import Control.Exception (assert)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Koriel.Core.Core as C
import Koriel.Core.Op
import Koriel.Core.Type hiding (Type)
import qualified Koriel.Core.Type as C
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Griff.DsEnv
import Language.Griff.Extension
import Language.Griff.Grouping
import Language.Griff.Prelude
import qualified Language.Griff.RnEnv as Rn
import Language.Griff.Syntax as G
import Language.Griff.TcEnv (TcEnv)
import qualified Language.Griff.TcEnv as Tc
import Language.Griff.Type as GT
import Language.Griff.TypeCheck (applySubst)
import qualified Text.PrettyPrint.HughesPJ as P

#ifdef DEBUG
import Debug.Trace (traceShowM)
#endif

-- | GriffからCoreへの変換
desugar ::
  (MonadUniq m, MonadFail m, MonadIO m, MonadGriff m) =>
  TcEnv ->
  BindGroup (Griff 'TypeCheck) ->
  m (DsEnv, Program (Id C.Type))
desugar tcEnv ds = do
  (dsEnv, prims) <- genPrimitive tcEnv
  (dsEnv', ds') <- runReaderT (dsBindGroup ds) dsEnv
  mainFuncDef <-
    mainFunc =<< runDef do
      _ <- bind $ searchMain $ Map.toList $ view varEnv dsEnv'
      pure (Atom $ C.Unboxed $ C.Int32 0)
  pure (dsEnv', Program (mainFuncDef : prims <> ds'))
  where
    -- エントリーポイントとなるmain関数を検索する
    searchMain ((griffId, coreId) : _) | griffId ^. idName == "main" && griffId ^. idIsGlobal = CallDirect coreId []
    searchMain (_ : xs) = searchMain xs
    searchMain _ = C.ExtCall "mainIsNotDefined" ([] :-> AnyT) []

-- 組み込み関数のCoreの生成
genPrimitive ::
  (MonadUniq m, MonadIO m, MonadFail m) =>
  TcEnv ->
  m (DsEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
genPrimitive env =
  execStateT ?? (DsEnv mempty env, []) $ do
    -- add_i32# : Int32#の和
    prim "add_i32#" $ \param -> do
      [x, y] <- destruct (Atom $ C.Var param) (C.Con "Tuple2" [C.Int32T, C.Int32T])
      pure $ BinOp Add x y
    -- add_i64# : Int64#の和
    prim "add_i64#" $ \param -> do
      [x, y] <- destruct (Atom $ C.Var param) (C.Con "Tuple2" [C.Int64T, C.Int64T])
      pure $ BinOp Add x y
  where
    prim name code = do
      nameId <- fromJust <$> use (_1 . tcEnv . Tc.rnEnv . Rn.varEnv . at name)
      Forall _ nameType <- fromJust <$> use (_1 . tcEnv . Tc.varEnv . at nameId)
      uniq <- getUniq
      nameId' <- newGlobalId (name <> show uniq) =<< dsType nameType
      _1 . varEnv . at nameId ?= nameId'
      case C.typeOf nameId' of
        -- プリミティブ関数は必ず一引数
        [paramType] :-> _ -> do
          param <- newId "$p" paramType
          fun <- runDef (code param)
          _2 %= ((nameId', ([param], fun)) :)
        _ -> bug Unreachable

-- BindGroupの脱糖衣
-- DataDef, Foreign, ScDefの順で処理する
dsBindGroup ::
  (MonadUniq m, MonadReader DsEnv m, MonadFail m, MonadIO m, MonadGriff m) =>
  BindGroup (Griff 'TypeCheck) ->
  m (DsEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
dsBindGroup bg = do
  (env, dataDefs') <- first mconcat <$> mapAndUnzipM dsDataDef (bg ^. dataDefs)
  local (env <>) $ do
    (env, foreigns') <- first mconcat <$> mapAndUnzipM dsForeign (bg ^. foreigns)
    local (env <>) $ do
      (env, scDefs') <- dsScDefGroup (bg ^. scDefs)
#ifdef DEBUG
      traceShowM . pPrint . Map.toList =<< view varEnv
#endif
      pure $ (env,) $ mconcat $ mconcat dataDefs' <> foreigns' <> scDefs'

-- 相互再帰するScDefのグループごとに脱糖衣する
dsScDefGroup ::
  (MonadUniq f, MonadReader DsEnv f, MonadFail f, MonadIO f, MonadGriff f) =>
  [[ScDef (Griff 'TypeCheck)]] ->
  f (DsEnv, [[(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]])
dsScDefGroup [] = (,[]) <$> ask
dsScDefGroup (ds : dss) = do
  (env, ds') <- dsScDefs ds
  local (env <>) $ do
    (env, dss') <- dsScDefGroup dss
    pure (env, ds' : dss')

-- 相互再帰的なグループをdesugar
dsScDefs ::
  (MonadUniq f, MonadReader DsEnv f, MonadFail f, MonadIO f, MonadGriff f) =>
  [ScDef (Griff 'TypeCheck)] ->
  f (DsEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
dsScDefs ds = do
  -- まず、このグループで宣言されているScDefの名前をすべて名前環境に登録する
  env <- foldMapA ?? ds $ \(_, f, _, _) -> do
    Just (Forall _ fType) <- view (tcEnv . Tc.varEnv . at f)
    f' <- newCoreId f =<< dsType fType
    pure $ mempty & varEnv .~ Map.singleton f f'
  local (env <>) $ (env,) <$> foldMapA dsScDef ds

dsScDef ::
  (MonadUniq f, MonadReader DsEnv f, MonadIO f, MonadFail f, MonadGriff f) =>
  ScDef (Griff 'TypeCheck) ->
  f [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]
dsScDef (WithType pos typ, name, params, expr) = do
  -- ScDefは関数かlazy valueでなくてはならない
  unless (GT._TyArr `has` typ || GT._TyLazy `has` typ) $
    errorOn pos $
      "Invalid Toplevel Declaration:"
        <+> P.quotes (pPrint name <+> ":" <+> pPrint typ)
  -- When typ is TyLazy{}, splitTyArr returns ([], typ).
  let (paramTypes, _) = splitTyArr typ
  params' <- zipWithM newCoreId params =<< traverse dsType paramTypes
  local (over varEnv (Map.fromList (zip params params') <>)) $ do
    name' <- lookupName name
    fun <- curryFun params' =<< dsExp expr
    pure [(name', fun)]

-- TODO: Griffのforeignでvoid型をあつかえるようにする #13
-- 1. Griffの型とCの型の相互変換を定義する
-- 2. 相互変換を値に対して行うCoreコードを生成する関数を定義する
-- 3. 2.の関数を使ってdsForeignを書き換える
dsForeign ::
  (MonadReader DsEnv f, MonadUniq f, MonadIO f) =>
  Foreign (Griff 'TypeCheck) ->
  f (DsEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
dsForeign (x@(WithType (_, primName) _), name, _) = do
  name' <- newCoreId name =<< dsType (x ^. toType)
  let (paramTypes, _) = splitTyArr (x ^. toType)
  params <- traverse (newId "$p" <=< dsType) paramTypes
  primType <- dsType (view toType x)
  fun <- curryFun params $ C.ExtCall primName primType (map C.Var params)
  pure (mempty & varEnv .~ Map.singleton name name', [(name', fun)])

dsDataDef ::
  (MonadUniq m, MonadReader DsEnv m, MonadFail m, MonadIO m) =>
  DataDef (Griff 'TypeCheck) ->
  m (DsEnv, [[(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]])
dsDataDef (_, name, _, cons) = fmap (first mconcat) $
  mapAndUnzipM ?? cons $ \(conName, _) -> do
    -- lookup constructor infomations
    Just (GT.TyCon name') <- preview (tcEnv . Tc.typeEnv . at name . _Just . Tc.constructor)
    conMap <- lookupConMap name' []
    let conType = fromJust $ List.lookup conName conMap

    -- desugar conType
    let (paramTypes, retType) = splitTyArr conType
    paramTypes' <- traverse dsType paramTypes
    retType' <- dsType retType

    -- generate constructor code
    conName' <- newCoreId conName $ buildsonType paramTypes' retType'
    ps <- traverse (newId "$p") paramTypes'
    expr <- runDef $ do
      unfoldedType <- unfoldType retType
      packed <- let_ unfoldedType (Pack unfoldedType (C.Con (conName ^. toText) paramTypes') $ map C.Var ps)
      pure $ Cast retType' packed
    obj <- case ps of
      [] -> pure ([], expr)
      _ -> curryFun ps expr
    pure (mempty & varEnv .~ Map.singleton conName conName', [(conName', obj)])
  where
    -- 引数のない値コンストラクタは、0引数のCore関数に変換される
    buildsonType [] retType = [] :-> retType
    buildsonType paramTypes retType = foldr (\a b -> [a] :-> b) retType paramTypes

-- Unboxedの脱糖衣
dsUnboxed :: G.Unboxed -> C.Unboxed
dsUnboxed (G.Int32 x) = C.Int32 $ toInteger x
dsUnboxed (G.Int64 x) = C.Int64 $ toInteger x
dsUnboxed (G.Float x) = C.Float x
dsUnboxed (G.Double x) = C.Double x
dsUnboxed (G.Char x) = C.Char x
dsUnboxed (G.String x) = C.String x

dsExp ::
  (HasCallStack, MonadUniq m, MonadReader DsEnv m, MonadIO m, MonadFail m) =>
  G.Exp (Griff 'TypeCheck) ->
  m (C.Exp (Id C.Type))
dsExp (G.Var x name) = do
  name' <- lookupName name
  -- Griffでの型とCoreでの型に矛盾がないかを検査
  -- Note: [0 argument]
  --   Core上で0引数関数で表現されるGriffの値は以下の二つ。
  --    1. {a}型の値（TyLazy）
  --    2. 引数のない値コンストラクタ
  --   このうち、2.は「0引数関数の呼び出し」の形でのみ出現する（dsExp G.Conの節参照）
  --   よって、ここではxがTyLazyのときのみname'が0引数関数になるはずである。
  case (x ^. toType, C.typeOf name') of
    -- TyLazyの型を検査
    (GT.TyLazy {}, [] :-> _) -> pure ()
    (GT.TyLazy {}, _) -> errorDoc $ "Invalid TyLazy:" <+> P.quotes (pPrint $ C.typeOf name')
    (_, [] :-> _) -> errorDoc $ "Invlalid type:" <+> P.quotes (pPrint name)
    _ -> pure ()
  if name' ^. idIsGlobal
    then do
      -- name（name'）がグローバルなとき、name'に対応する適切な値（クロージャ）は存在しない。
      -- そこで、name'の値が必要になったときに、都度クロージャを生成する。
      clsId <- newId "$gblcls" (C.typeOf name')
      ps <- case C.typeOf name' of
        pts :-> _ -> traverse (newId "$p") pts
        _ -> bug Unreachable
      pure $ C.Let [(clsId, Fun ps $ CallDirect name' $ map C.Var ps)] $ Atom $ C.Var clsId
    else pure $ Atom $ C.Var name'
dsExp (G.Con _ name) = do
  name' <- lookupName name
  case C.typeOf name' of
    -- 値コンストラクタ名は全部global。
    -- 引数のない値コンストラクタは、0引数関数の呼び出しに変換する。
    [] :-> _ -> pure $ CallDirect name' []
    -- グローバルな関数と同様に、引数のある値コンストラクタも、
    -- 値が必要になったときに都度クロージャを生成する。
    pts :-> _ -> do
      clsId <- newId "$concls" (C.typeOf name')
      ps <- traverse (newId "$p") pts
      pure $ C.Let [(clsId, Fun ps $ CallDirect name' $ map C.Var ps)] $ Atom $ C.Var clsId
    _ -> bug Unreachable
dsExp (G.Unboxed _ u) = pure $ Atom $ C.Unboxed $ dsUnboxed u
dsExp (G.Apply _ f x) = runDef $ do
  f' <- bind =<< dsExp f
  case C.typeOf f' of
    [xType] :-> _ -> do
      -- Note: [Cast Argument Type]
      --   x の型と f の引数の型は必ずしも一致しない
      --   適切な型にcastする必要がある
      x' <- cast xType =<< dsExp x
      pure $ Call f' [x']
    _ -> bug Unreachable
dsExp (G.OpApp _ op x y) = runDef $ do
  op' <- lookupName op
  case C.typeOf op' of
    [xType] :-> ([yType] :-> _) -> do
      -- Ref: [Cast Argument Type]
      x' <- cast xType =<< dsExp x
      y' <- cast yType =<< dsExp y
      e1 <-
        if op' ^. idIsGlobal
          then bind (CallDirect op' [x'])
          else bind (Call (C.Var op') [x'])
      pure $ Call e1 [y']
    _ -> bug Unreachable
dsExp (G.Fn x (Clause _ [] ss : _)) = do
  -- lazy valueの脱糖衣
  ss' <- dsStmts ss
  typ <- dsType (x ^. toType)
  runDef do
    fun <- let_ typ $ Fun [] ss'
    pure $ Atom fun
dsExp (G.Fn x cs@(Clause _ ps es : _)) = do
  ps' <- traverse (\p -> newId "$p" =<< dsType (p ^. toType)) ps
  typ <- dsType (last es ^. toType)
  -- destruct Clauses
  -- 各節のパターン列を行列に見立て、転置してmatchにわたし、パターンを分解する
  -- 例えば、{ f Nil -> f empty | f (Cons x xs) -> f x }の場合は、
  -- [ [f, Nil], [f, Cons x xs] ] に見立て、
  -- [ [f, f], [Nil, Cons x xs] ] に転置する
  (pss, es) <-
    first List.transpose
      <$> mapAndUnzipM
        ( \(Clause _ ps es) ->
            pure (ps, dsStmts es)
        )
        cs
  body <- match ps' pss es (Error typ)
  obj <- curryFun ps' body
  v <- newId "$fun" =<< dsType (x ^. toType)
  pure $ C.Let [(v, uncurry Fun obj)] $ Atom $ C.Var v
dsExp (G.Fn _ []) = bug Unreachable
dsExp (G.Tuple _ es) = runDef $ do
  es' <- traverse (bind <=< dsExp) es
  let con = C.Con ("Tuple" <> length es ^. toText) $ map C.typeOf es'
  let ty = SumT $ Set.singleton con
  tuple <- let_ ty $ Pack ty con es'
  pure $ Atom tuple
dsExp (G.Force _ e) = runDef $ do
  -- lazy valueは0引数関数に変換されるので、その評価は0引数関数の呼び出しになる
  e' <- bind =<< dsExp e
  pure $ Call e' []

dsStmts :: (MonadUniq m, MonadReader DsEnv m, MonadIO m, MonadFail m) => [Stmt (Griff 'TypeCheck)] -> m (C.Exp (Id C.Type))
dsStmts [] = bug Unreachable
dsStmts [NoBind _ e] = dsExp e
dsStmts [G.Let _ _ e] = dsExp e
dsStmts (NoBind _ e : ss) = runDef $ do
  _ <- bind =<< dsExp e
  dsStmts ss
dsStmts (G.Let _ v e : ss) = do
  e' <- dsExp e
  v' <- newId ("$let_" <> v ^. idName) (C.typeOf e')
  local (over varEnv $ Map.insert v v') do
    ss' <- dsStmts ss
    pure $ Match e' (Bind v' ss' :| [])

-- TODO: The Implementation of Functional Programming Languages
-- を元にコメントを追加

-- パターンマッチを分解し、switch-case相当の分岐で表現できるように変換する
match ::
  HasCallStack =>
  (MonadReader DsEnv m, MonadFail m, MonadIO m, MonadUniq m) =>
  [Id C.Type] ->
  [[Pat (Griff 'TypeCheck)]] ->
  [m (C.Exp (Id C.Type))] ->
  C.Exp (Id C.Type) ->
  m (C.Exp (Id C.Type))
match (u : us) (ps : pss) es err
  -- Variable Rule
  -- パターンの先頭がすべて変数のとき
  | all (has _VarP) ps =
    {- Note: How to implement the Variable Rule?
        There are two (old) implementations.
        I believe that the Original impl is correct.
        But I'm not sure if this is correct.
        So, the `assert` is inserted in code.
        If I'm wrong, this `assert` is going to fail one day.
    -}
    -- -- Cast version
    -- match us pss (zipWith (\(VarP x v) e -> runDef $ do
    --   ty' <- dsType (x ^. toType)
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
              -- 変数パターンvについて、式中に現れるすべてのvをパターンマッチ対象のuで置き換える
              VarP x v -> \e -> do
                patTy <- dsType (x ^. toType)
                -- If this assert fail, there are some bug about polymorphic type.
                -- Ref: How to implement the Variable Rule?
                assert (patTy == C.typeOf u) $ pure ()
                local (over varEnv (Map.insert v u)) e
              _ -> bug Unreachable
          )
          ps
          es
      )
      err
  -- Constructor Rule
  -- パターンの先頭がすべて値コンストラクタのとき
  | all (has _ConP) ps = do
    let patType = head ps ^. toType
    unless (_TyApp `has` patType || _TyCon `has` patType) $
      errorDoc $ "Not valid type:" <+> pPrint patType
    -- 型からコンストラクタの集合を求める
    let (con, ts) = splitCon patType
    conMap <- lookupConMap con ts
    -- TODO: csとcasesのdoを結合
    cs <- for conMap $ \(conName, conType) -> do
      paramTypes <- traverse dsType $ fst $ splitTyArr conType
      let ccon = C.Con (conName ^. toText) paramTypes
      params <- traverse (newId "$p") paramTypes
      pure (conName, ccon, params)
    -- 各コンストラクタごとにC.Caseを生成する
    cases <- for cs $ \(gcon, ccon, params) -> do
      -- パターン行列（未転置）
      let (pss', es') = unzip $ group gcon (List.transpose (ps : pss)) es
      Unpack ccon params <$> match (params <> us) (List.transpose pss') es' err
    unfoldedType <- unfoldType patType
    pure $ Match (Cast unfoldedType $ C.Var u) $ NonEmpty.fromList cases
  -- パターンの先頭がすべてタプルのとき
  | all (has _TupleP) ps = do
    let patType = head ps ^. toType
    SumT [con@(C.Con _ ts)] <- dsType patType
    params <- traverse (newId "$p") ts
    cases <- do
      let (pss', es') = unzip $ groupTuple (List.transpose (ps : pss)) es
      (:| []) . Unpack con params <$> match (params <> us) (List.transpose pss') es' err
    pure $ Match (Atom $ C.Var u) cases
  -- パターンの先頭がすべてunboxedな値のとき
  | all (has _UnboxedP) ps = do
    let cs =
          map
            ( \case
                UnboxedP _ x -> dsUnboxed x
                _ -> bug Unreachable
            )
            ps
    cases <- traverse (\c -> Switch c <$> match us pss es err) cs
    -- `_ -> err` のパターンはerrに簡約されているので、
    -- ここで新たに$_を生成し、Bindパターンを生成する必要がある
    hole <- newId "$_" (C.typeOf u)
    pure $ Match (Atom $ C.Var u) $ NonEmpty.fromList (cases <> [C.Bind hole err])
  -- The Mixture Rule
  -- 複数種類のパターンが混ざっているとき
  | otherwise =
    do
      let ((ps', ps''), (pss', pss''), (es', es'')) = partition ps pss es
      err' <- match (u : us) (ps'' : pss'') es'' err
      match (u : us) (ps' : pss') es' err'
  where
    -- Mixture Rule以外にマッチするようにパターン列を分解
    partition [] _ _ = bug Unreachable
    partition ps@(VarP {} : _) pss es =
      let (ps', ps'') = span (has _VarP) ps
       in ((ps', ps''), unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
    partition ps@(ConP {} : _) pss es =
      let (ps', ps'') = span (has _ConP) ps
       in ((ps', ps''), unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
    partition ps@(TupleP {} : _) pss es =
      let (ps', ps'') = span (has _TupleP) ps
       in ((ps', ps''), unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
    partition ps@(UnboxedP {} : _) pss es =
      let (ps', ps'') = span (has _UnboxedP) ps
       in ((ps', ps''), unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
    -- コンストラクタgconの引数部のパターンpsを展開したパターン行列を生成する
    group gcon pss' es = mapMaybe (aux gcon) (zip pss' es)
      where
        aux gcon (ConP _ gcon' ps : pss, e)
          | gcon == gcon' = Just (ps <> pss, e)
          | otherwise = Nothing
        aux _ (p : _, _) = errorDoc $ "Invalid pattern:" <+> pPrint p
        aux _ ([], _) = bug Unreachable
    groupTuple pss' es = zipWith aux pss' es
      where
        aux (TupleP _ ps : pss) e = (ps <> pss, e)
        aux (p : _) _ = errorDoc $ "Invalid pattern:" <+> pPrint p
        aux [] _ = bug Unreachable
match [] [] (e : _) _ = e
match _ [] [] err = pure err
match u pss es err = do
  errorDoc $ "match" <+> pPrint u <+> pPrint pss <+> pPrint (length es) <+> pPrint err

-- Griffの型をCoreの型に変換する
dsType :: (HasCallStack, MonadIO m) => GT.Type -> m C.Type
dsType t@GT.TyApp {} = do
  let (con, ts) = splitCon t
  DataT (con ^. toText) <$> traverse dsType ts
dsType (GT.TyVar _) = pure AnyT
dsType (GT.TyCon con)
  | kind con == Star = pure $ DataT (con ^. toText) []
  | otherwise = errorDoc $ "Invalid kind:" <+> pPrint con <+> ":" <+> pPrint (kind con)
dsType (GT.TyPrim GT.Int32T) = pure C.Int32T
dsType (GT.TyPrim GT.Int64T) = pure C.Int64T
dsType (GT.TyPrim GT.FloatT) = pure C.FloatT
dsType (GT.TyPrim GT.DoubleT) = pure C.DoubleT
dsType (GT.TyPrim GT.CharT) = pure C.CharT
dsType (GT.TyPrim GT.StringT) = pure C.StringT
dsType (GT.TyArr t1 t2) = do
  t1' <- dsType t1
  t2' <- dsType t2
  pure $ [t1'] :-> t2'
dsType (GT.TyTuple ts) =
  SumT . Set.singleton . C.Con ("Tuple" <> length ts ^. toText) <$> traverse dsType ts
dsType (GT.TyLazy t) = ([] :->) <$> dsType t
dsType (GT.TyMeta tv) = do
  mtype <- GT.readMetaTv tv
  case mtype of
    Just t -> dsType t
    Nothing -> error "TyMeta must be removed"

-- List aのような型を、<Nil | Cons a (List a)>のような和型に展開する
unfoldType :: (MonadReader DsEnv m, MonadFail m, MonadIO m) => GT.Type -> m C.Type
unfoldType t | GT._TyApp `has` t
                 || t ^? GT._TyCon . to kind == Just Star =
  do
    let (con, ts) = splitCon t
    conMap <- lookupConMap con ts
    SumT
      <$> foldMapA
        ( \(conName, conType) ->
            Set.singleton . C.Con (conName ^. toText) <$> traverse dsType (fst $ splitTyArr conType)
        )
        conMap
unfoldType t = dsType t

-- Desugar Monad

lookupName :: (HasCallStack, MonadReader DsEnv m) => TcId -> m (Id C.Type)
lookupName name = do
  mname' <- view (varEnv . at name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> P.quotes (pPrint name)

lookupConMap ::
  (MonadReader DsEnv m, MonadFail m) =>
  Id Kind ->
  [GT.Type] ->
  m [(TcId, GT.Type)]
lookupConMap con ts = do
  typeEnv <- view (tcEnv . Tc.typeEnv)
  case List.find (\Tc.TypeDef {Tc._constructor = t} -> t == GT.TyCon con) (Map.elems typeEnv) of
    Just Tc.TypeDef {Tc._qualVars = as, Tc._union = conMap} ->
      pure $ over (mapped . _2) (applySubst $ Map.fromList $ zip as ts) conMap
    Nothing -> errorDoc $ "Not in scope:" <+> P.quotes (pPrint con)

newCoreId :: MonadUniq f => Id ModuleName -> a -> f (Id a)
newCoreId griffId coreType =
  newId (griffId ^. idMeta . _Module <> "." <> griffId ^. idName) coreType
    <&> idIsGlobal .~ griffId ^. idIsGlobal

-- 関数をカリー化する
curryFun ::
  (HasCallStack, MonadUniq m) =>
  [Id C.Type] ->
  C.Exp (Id C.Type) ->
  m ([Id C.Type], C.Exp (Id C.Type))
-- FIXME: curryFun [] e の正しい処理は、eの型に応じて引数リストpsを生成し、(ps, (apply e ps))を返す
-- そのためには、Coreの項に明示的な型の適用を追加する必要がある
-- curryFun [] e =
--   case C.typeOf e of
--     pts :-> _ -> do
--       ps <- traverse (newId "$p") pts
--       curryFun ps =<< runDef (Call <$> bind e <*> pure (map C.Var ps))
--     t -> errorDoc $ "Invalid type:" <+> P.quotes (pPrint t)
curryFun [] e@(C.Let ds (Atom (C.Var v))) = case List.lookup v ds of
  Just (Fun ps e) | not $ any ((/= v) . fst) ds -> pure (ps, e)
  _ -> errorDoc $ "Invalid expression:" <+> P.quotes (pPrint e)
curryFun [] e = errorDoc $ "Invalid expression:" <+> P.quotes (pPrint e)
curryFun [x] e = pure ([x], e)
curryFun ps@(_ : _) e = curryFun' ps []
  where
    curryFun' [] _ = bug Unreachable
    curryFun' [x] as = do
      x' <- newId (x ^. idName) (C.typeOf x)
      fun <- newId "$curry" (C.typeOf $ Fun ps e)
      let body = C.Call (C.Var fun) $ reverse $ C.Var x' : as
      pure ([x'], C.Let [(fun, Fun ps e)] body)
    curryFun' (x : xs) as = do
      x' <- newId (x ^. idName) (C.typeOf x)
      fun <- curryFun' xs (C.Var x' : as)
      let funObj = uncurry Fun fun
      body <- runDef $ do
        fun <- let_ (C.typeOf funObj) funObj
        pure $ Atom fun
      pure ([x'], body)
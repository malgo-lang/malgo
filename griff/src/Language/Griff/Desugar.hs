{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import Koriel.Prelude
import Koriel.Pretty
import Language.Griff.Extension
import Language.Griff.Grouping
import qualified Language.Griff.RnEnv as Rn
import Language.Griff.Syntax as G
import Language.Griff.TcEnv (TcEnv)
import qualified Language.Griff.TcEnv as Tc
import Language.Griff.Type as GT
import qualified Language.Griff.Typing as Typing
import qualified Text.PrettyPrint.HughesPJ as P

#ifdef DEBUG
import Debug.Trace (traceShowM)
#endif

-- 脱糖衣処理の環境
data DesugarEnv = DesugarEnv
  { _varEnv :: Map TcId (Id C.Type), -- ^ Griff -> Coreの名前環境
    _tcEnv :: TcEnv -- ^ 型環境
  }
  deriving stock (Show)

instance Semigroup DesugarEnv where
  DesugarEnv v1 t1 <> DesugarEnv v2 t2 = DesugarEnv (v1 <> v2) (t1 <> t2)

instance Monoid DesugarEnv where
  mempty = DesugarEnv mempty mempty

varEnv :: Lens' DesugarEnv (Map TcId (Id C.Type))
varEnv = lens _varEnv (\e x -> e {_varEnv = x})

tcEnv :: Lens' DesugarEnv TcEnv
tcEnv = lens _tcEnv (\e x -> e {_tcEnv = x})

-- | GriffからCoreへの変換
desugar ::
  (MonadUniq m, MonadFail m, MonadIO m) =>
  TcEnv ->
  BindGroup (Griff 'TypeCheck) ->
  m (Program (Id C.Type))
desugar tcEnv ds = do
  (dcEnv, prims) <- genPrimitive tcEnv
  (dcEnv', ds') <- runReaderT (dcBindGroup ds) dcEnv
  pure $ Program (prims <> ds') $ searchMain $ Map.toList $ view varEnv dcEnv'
  where
    -- エントリーポイントとなるmain関数を検索する
    searchMain ((griffId, coreId) : _) | griffId ^. idName == "main" && griffId ^. idIsGlobal = CallDirect coreId []
    searchMain (_ : xs) = searchMain xs
    searchMain _ = C.ExtCall "mainIsNotDefined" ([] :-> AnyT) []

-- 組み込み関数のCoreの生成
genPrimitive ::
  (MonadUniq m, MonadIO m, MonadFail m) =>
  TcEnv ->
  m (DesugarEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
genPrimitive env = do
  execStateT ?? (DesugarEnv mempty env, []) $ do
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
      nameId' <- newGlobalId (name <> show uniq) =<< dcType nameType
      fun <- case C.typeOf nameId' of
        -- プリミティブ関数は必ず一引数
        [paramType] :-> _ -> do
          param <- newId "$p" paramType
          fmap ([param],) $ runDef (code param)
        _ -> bug Unreachable
      _2 %= ((nameId', fun):)
      _1 . varEnv . at nameId ?= nameId'

-- BindGroupの脱糖衣
-- DataDef, Foreign, ScDefの順で処理する
dcBindGroup ::
  (MonadUniq m, MonadReader DesugarEnv m, MonadFail m, MonadIO m) =>
  BindGroup (Griff 'TypeCheck) ->
  m (DesugarEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
dcBindGroup bg = do
  (env, dataDefs') <- first mconcat <$> mapAndUnzipM dcDataDef (bg ^. dataDefs)
  local (env <>) $ do
    (env, foreigns') <- first mconcat <$> mapAndUnzipM dcForeign (bg ^. foreigns)
    local (env <>) $ do
      (env, scDefs') <- dcScDefGroup (bg ^. scDefs)
#ifdef DEBUG
      traceShowM . pPrint . Map.toList =<< view varEnv
#endif
      pure $ (env,) $ mconcat $ mconcat dataDefs' <> foreigns' <> scDefs'

-- 相互再帰するScDefのグループごとに脱糖衣する
dcScDefGroup ::
  (MonadUniq f, MonadReader DesugarEnv f, MonadFail f, MonadIO f) =>
  [[ScDef (Griff 'TypeCheck)]] ->
  f (DesugarEnv, [[(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]])
dcScDefGroup [] = (,[]) <$> ask
dcScDefGroup (ds : dss) = do
  (env, ds') <- dcScDefs ds
  local (env <>) $ do
    (env, dss') <- dcScDefGroup dss
    pure (env, ds' : dss')

-- 相互再帰的なグループをdesugar
dcScDefs ::
  (MonadUniq f, MonadReader DesugarEnv f, MonadFail f, MonadIO f) =>
  [ScDef (Griff 'TypeCheck)] ->
  f (DesugarEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
dcScDefs ds = do
  -- まず、このグループで宣言されているScDefの名前をすべて名前環境に登録する
  env <- foldMapA ?? ds $ \(_, f, _, _) -> do
    Just (Forall _ fType) <- asks $ view (tcEnv . Tc.varEnv . at f)
    f' <- newGlobalId (f ^. idMeta . _Package <> "." <> f ^. idName) =<< dcType fType
    pure $ mempty & varEnv .~ Map.singleton f f'
  local (env <>) $ (env,) <$> foldMapA dcScDef ds

dcScDef ::
  (MonadUniq f, MonadReader DesugarEnv f, MonadIO f, MonadFail f) =>
  ScDef (Griff 'TypeCheck) ->
  f [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]
dcScDef (WithType pos typ, name, params, expr) = do
  -- ScDefは関数かlazy valueでなくてはならない
  unless (GT._TyArr `has` typ || GT._TyLazy `has` typ) $
    errorOn pos $
      "Invalid Toplevel Declaration:"
        <+> P.quotes (pPrint name <+> ":" <+> pPrint typ)
  -- When typ is TyLazy{}, splitTyArr returns ([], typ).
  let (paramTypes, _) = splitTyArr typ
  params' <- zipWithM newCoreId params =<< traverse dcType paramTypes
  local (over varEnv (Map.fromList (zip params params') <>)) $ do
    name' <- lookupName name
    fun <- curryFun params' =<< dcExp expr
    pure [(name', fun)]

-- TODO: Griffのforeignでvoid型をあつかえるようにする #13
-- 1. Griffの型とCの型の相互変換を定義する
-- 2. 相互変換を値に対して行うCoreコードを生成する関数を定義する
-- 3. 2.の関数を使ってdcForeignを書き換える
dcForeign ::
  (MonadReader DesugarEnv f, MonadUniq f, MonadIO f) =>
  Foreign (Griff 'TypeCheck) ->
  f (DesugarEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
dcForeign (x@(WithType (_, primName) _), name, _) = do
  name' <- newCoreId name =<< dcType (x ^. toType)
  let (paramTypes, _) = splitTyArr (x ^. toType)
  params <- traverse (newId "$p" <=< dcType) paramTypes
  primType <- dcType (view toType x)
  fun <- curryFun params $ C.ExtCall primName primType (map C.Var params)
  pure (mempty & varEnv .~ Map.singleton name name', [(name', fun)])

dcDataDef ::
  (MonadUniq m, MonadReader DesugarEnv m, MonadFail m, MonadIO m) =>
  DataDef (Griff 'TypeCheck) ->
  m (DesugarEnv, [[(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]])
dcDataDef (_, name, _, cons) = fmap (first mconcat) $
  mapAndUnzipM ?? cons $ \(conName, _) -> do
    -- lookup constructor infomations
    Just (GT.TyCon name') <- asks $ view (tcEnv . Tc.typeEnv . at name)
    conMap <- lookupConMap name' []
    let conType = fromJust $ List.lookup conName conMap

    -- desugar conType
    let (paramTypes, retType) = splitTyArr conType
    paramTypes' <- traverse dcType paramTypes
    retType' <- dcType retType

    -- generate constructor code
    conName' <- newCoreId conName $ buildConType paramTypes' retType'
    ps <- traverse (newId "$p") paramTypes'
    expr <- runDef $ do
      unfoldedType <- unfoldType retType
      packed <- let_ unfoldedType (Pack unfoldedType (C.Con (conName ^. toText) paramTypes') $ map C.Var ps)
      pure $ Cast retType' packed
    obj <- curryFun ps expr
    pure (mempty & varEnv .~ Map.singleton conName conName', [(conName', obj)])
  where
    -- 引数のない値コンストラクタは、0引数のCore関数に変換される
    buildConType [] retType = [] :-> retType
    buildConType paramTypes retType = foldr (\a b -> [a] :-> b) retType paramTypes

-- Unboxedの脱糖衣
dcUnboxed :: G.Unboxed -> C.Unboxed
dcUnboxed (G.Int32 x) = C.Int32 $ toInteger x
dcUnboxed (G.Int64 x) = C.Int64 $ toInteger x
dcUnboxed (G.Float x) = C.Float x
dcUnboxed (G.Double x) = C.Double x
dcUnboxed (G.Char x) = C.Char x
dcUnboxed (G.String x) = C.String x

dcExp ::
  (HasCallStack, MonadUniq m, MonadReader DesugarEnv m, MonadIO m, MonadFail m) =>
  G.Exp (Griff 'TypeCheck) ->
  m (C.Exp (Id C.Type))
dcExp (G.Var x name) = do
  name' <- lookupName name
  -- Griffでの型とCoreでの型に矛盾がないかを検査
  -- Note: [0 argument]
  --   Core上で0引数関数で表現されるGriffの値は以下の二つ。
  --    1. {a}型の値（TyLazy）
  --    2. 引数のない値コンストラクタ
  --   このうち、2.は「0引数関数の呼び出し」の形でのみ出現する（dcExp G.Conの節参照）
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
      pure $ Let [(clsId, Fun ps $ CallDirect name' $ map C.Var ps)] $ Atom $ C.Var clsId
    else pure $ Atom $ C.Var name'
dcExp (G.Con _ name) = do
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
      pure $ Let [(clsId, Fun ps $ CallDirect name' $ map C.Var ps)] $ Atom $ C.Var clsId
    _ -> bug Unreachable
dcExp (G.Unboxed _ u) = pure $ Atom $ C.Unboxed $ dcUnboxed u
dcExp (G.Apply _ f x) = runDef $ do
  f' <- bind =<< dcExp f
  case C.typeOf f' of
    [xType] :-> _ -> do
      -- Note: [Cast Argument Type]
      --   x の型と f の引数の型は必ずしも一致しない
      --   適切な型にcastする必要がある
      x' <- cast xType =<< dcExp x
      pure $ Call f' [x']
    _ -> bug Unreachable
dcExp (G.OpApp _ op x y) = runDef $ do
  op' <- lookupName op
  case C.typeOf op' of
    [xType] :-> ([yType] :-> _) -> do
      -- Ref: [Cast Argument Type]
      x' <- cast xType =<< dcExp x
      y' <- cast yType =<< dcExp y
      e1 <-
        if op' ^. idIsGlobal
          then bind (CallDirect op' [x'])
          else bind (Call (C.Var op') [x'])
      pure $ Call e1 [y']
    _ -> bug Unreachable
dcExp (G.Fn x (Clause _ [] e : _)) = runDef $ do
  -- lazy valueの脱糖衣
  e' <- dcExp e
  typ <- dcType (x ^. toType)
  Atom <$> let_ typ (Fun [] e')
dcExp (G.Fn x cs@(Clause _ ps e : _)) = do
  ps' <- traverse (\p -> newId "$p" =<< dcType (p ^. toType)) ps
  typ <- dcType (e ^. toType)
  -- destruct Clauses
  -- 各節のパターン列を行列に見立て、転置してmatchにわたし、パターンを分解する
  -- 例えば、{ f Nil -> f empty | f (Cons x xs) -> f x }の場合は、
  -- [ [f, Nil], [f, Cons x xs] ] に見立て、
  -- [ [f, f], [Nil, Cons x xs] ] に転置する
  (pss, es) <- first List.transpose <$> mapAndUnzipM (\(Clause _ ps e) -> pure (ps, dcExp e)) cs
  body <- match ps' pss es (Error typ)
  obj <- curryFun ps' body
  v <- newId "$fun" =<< dcType (x ^. toType)
  pure $ Let [(v, uncurry Fun obj)] $ Atom $ C.Var v
dcExp (G.Fn _ []) = bug Unreachable
dcExp (G.Tuple _ es) = runDef $ do
  es' <- traverse (bind <=< dcExp) es
  let con = C.Con ("Tuple" <> length es ^. toText) $ map C.typeOf es'
  let ty = SumT $ Set.singleton con
  tuple <- let_ ty $ Pack ty con es'
  pure $ Atom tuple
dcExp (G.Force _ e) = runDef $ do
  -- lazy valueは0引数関数に変換されるので、その評価は0引数関数の呼び出しになる
  e' <- bind =<< dcExp e
  pure $ Call e' []

-- TODO: The Implementation of Functional Programming Languages
-- を元にコメントを追加

-- パターンマッチを分解し、switch-case相当の分岐で表現できるように変換する
match ::
  HasCallStack =>
  (MonadReader DesugarEnv m, MonadFail m, MonadIO m, MonadUniq m) =>
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
    --   ty' <- dcType (x ^. toType)
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
                patTy <- dcType (x ^. toType)
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
      paramTypes <- traverse dcType $ fst $ splitTyArr conType
      let ccon = C.Con (conName ^. toText) paramTypes
      params <- traverse (newId "$p") paramTypes
      pure (conName, ccon, params)
    -- 各コンストラクタごとにC.Caseを生成する
    cases <- for cs $ \(gcon, ccon, params) -> do
      let (pss', es') = unzip $ group gcon (List.transpose (ps : pss)) es
      Unpack ccon params <$> match (params <> us) (List.transpose pss') es' err
    unfoldedType <- unfoldType patType
    pure $ Match (Cast unfoldedType $ C.Var u) $ NonEmpty.fromList cases
  -- パターンの先頭がすべてunboxedな値のとき
  | all (has _UnboxedP) ps = do
    let cs =
          map
            ( \case
                UnboxedP _ x -> dcUnboxed x
                _ -> bug Unreachable
            )
            ps
    cases <- traverse (\c -> Switch c <$> match us pss es err) cs
    hole <- newId "$_" (C.typeOf u)
    pure $ Match (Atom $ C.Var u) $ NonEmpty.fromList (cases <> [C.Bind hole err])
  -- The Mixture Rule
  -- 複数種類のパターンが混ざっているとき
  | otherwise =
    do
      let ((ps', ps''), (pss', pss''), (es', es'')) = partition ps pss es
      match (u : us) (ps' : pss') es' =<< match (u : us) (ps'' : pss'') es'' err
  where
    -- Mixture Rule以外にマッチするようにパターン列を分解
    partition [] _ _ = bug Unreachable
    partition ps@(VarP {} : _) pss es =
      let (ps', ps'') = span (has _VarP) ps
       in ((ps', ps''), unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
    partition ps@(ConP {} : _) pss es =
      let (ps', ps'') = span (has _ConP) ps
       in ((ps', ps''), unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
    partition ps@(UnboxedP {} : _) pss es =
      let (ps', ps'') = span (has _UnboxedP) ps
       in ((ps', ps''), unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
    group gcon pss' es = mapMaybe (aux gcon) (zip pss' es)
      where
        aux gcon (ConP _ gcon' ps : pss, e)
          | gcon == gcon' = Just (ps <> pss, e)
          | otherwise = Nothing
        aux _ (p : _, _) = errorDoc $ "Invalid pattern:" <+> pPrint p
        aux _ ([], _) = bug Unreachable
match [] [] (e : _) _ = e
match _ [] [] err = pure err
match _ _ _ _ = bug Unreachable

-- Griffの型をCoreの型に変換する
dcType :: (HasCallStack, MonadIO m) => GT.Type -> m C.Type
dcType t@GT.TyApp {} = do
  let (con, ts) = splitCon t
  DataT (con ^. toText) <$> traverse dcType ts
dcType (GT.TyVar _) = pure AnyT
dcType (GT.TyCon con)
  | kind con == Star = pure $ DataT (con ^. toText) []
  | otherwise = errorDoc $ "Invalid kind:" <+> pPrint con <+> ":" <+> pPrint (kind con)
dcType (GT.TyPrim GT.Int32T) = pure C.Int32T 
dcType (GT.TyPrim GT.Int64T) = pure C.Int64T
dcType (GT.TyPrim GT.FloatT) = pure C.FloatT
dcType (GT.TyPrim GT.DoubleT) = pure C.DoubleT
dcType (GT.TyPrim GT.CharT) = pure C.CharT
dcType (GT.TyPrim GT.StringT) = pure C.StringT
dcType (GT.TyArr t1 t2) = do
  t1' <- dcType t1
  t2' <- dcType t2
  pure $ [t1'] :-> t2'
dcType (GT.TyTuple ts) =
  SumT . Set.singleton . C.Con ("Tuple" <> length ts ^. toText) <$> traverse dcType ts
dcType (GT.TyLazy t) = ([] :->) <$> dcType t
dcType (GT.TyMeta tv) = do
  mtype <- Typing.readMetaTv tv
  case mtype of
    Just t -> dcType t
    Nothing -> error "TyMeta must be removed"

-- List aのような型を、<Nil | Cons a (List a)>のような和型に展開する
unfoldType :: (MonadReader DesugarEnv m, MonadFail m, MonadIO m) => GT.Type -> m C.Type
unfoldType t | GT._TyApp `has` t
                 || t ^? GT._TyCon . to kind == Just Star =
  do
    let (con, ts) = splitCon t
    conMap <- lookupConMap con ts
    SumT
      <$> foldMapA
        ( \(conName, conType) ->
            Set.singleton . C.Con (conName ^. toText) <$> traverse dcType (fst $ splitTyArr conType)
        )
        conMap
unfoldType t = dcType t

-- Desugar Monad

lookupName :: (HasCallStack, MonadReader DesugarEnv m) => TcId -> m (Id C.Type)
lookupName name = do
  mname' <- asks $ view (varEnv . at name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> P.quotes (pPrint name)

lookupConMap ::
  (MonadReader DesugarEnv m, MonadFail m) =>
  Id Kind ->
  [GT.Type] ->
  m [(TcTId, GT.Type)]
lookupConMap con ts = do
  Just (as, conMap) <- asks $ view (tcEnv . Tc.tyConEnv . at con)
  pure $ over (mapped . _2) (Typing.applySubst $ Map.fromList $ zip as ts) conMap

newCoreId :: MonadUniq m => Id Package -> C.Type -> m (Id C.Type)
newCoreId griffId coreType = do
  coreId <- newId (griffId ^. idMeta . _Package <> "." <> griffId ^. idName) coreType
  pure $ coreId & idIsGlobal .~ griffId ^. idIsGlobal

-- 関数をカリー化する
curryFun ::
  (HasCallStack, MonadUniq m) =>
  [Id C.Type] ->
  C.Exp (Id C.Type) ->
  m ([Id C.Type], C.Exp (Id C.Type))
curryFun [] e@(Let ds (Atom (C.Var v))) = case List.lookup v ds of
  Just (Fun ps e) | not $ any ((/= v) . fst) ds -> pure (ps, e)
  _ -> errorDoc $ "Invalid expression:" <+> P.quotes (pPrint e)
curryFun [] e = pure ([], e)
curryFun [x] e = pure ([x], e)
curryFun ps@(_ : _) e = curryFun' ps []
  where
    curryFun' [] _ = bug Unreachable
    curryFun' [x] as = do
      x' <- newId (x ^. idName) (C.typeOf x)
      fun <- newId "$curry" (C.typeOf $ Fun ps e)
      let body = C.Call (C.Var fun) $ reverse $ C.Var x' : as
      pure ([x'], Let [(fun, Fun ps e)] body)
    curryFun' (x : xs) as = do
      x' <- newId (x ^. idName) (C.typeOf x)
      fun <- curryFun' xs (C.Var x' : as)
      let funObj = uncurry Fun fun
      body <- runDef $ do
        fun <- let_ (C.typeOf funObj) funObj
        pure $ Atom fun
      pure ([x'], body)

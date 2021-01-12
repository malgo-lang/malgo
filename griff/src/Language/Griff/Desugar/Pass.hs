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
module Language.Griff.Desugar.Pass (desugar) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Koriel.Core.Core as C
import Koriel.Core.Type hiding (Type)
import qualified Koriel.Core.Type as C
import Koriel.Id hiding (newGlobalId, newId)
import qualified Koriel.Id as Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Griff.Desugar.DsEnv
import Language.Griff.Interface
import Language.Griff.Prelude
import Language.Griff.Syntax as G
import Language.Griff.Syntax.Extension
import Language.Griff.Type as GT
import Language.Griff.TypeCheck.Pass (applySubst)
import Language.Griff.TypeCheck.TcEnv (TcEnv)
import qualified Language.Griff.TypeCheck.TcEnv as Tc

-- | GriffからCoreへの変換
desugar ::
  (MonadUniq m, MonadFail m, MonadIO m, MonadGriff m) =>
  TcEnv ->
  Module (Griff 'TypeCheck) ->
  m (DsEnv, Program (Id C.Type))
desugar tcEnv (Module modName ds) = do
  (dsEnv', ds') <- runReaderT (dsBindGroup ds) (DsEnv modName mempty tcEnv)
  case searchMain (Map.toList $ view varEnv dsEnv') of
    Just mainCall -> do
      mainFuncDef <-
        mainFunc =<< runDef do
          _ <- bind mainCall
          pure (Atom $ C.Unboxed $ C.Int32 0)
      pure (dsEnv', Program (mainFuncDef : ds'))
    Nothing -> pure (dsEnv', Program ds')
  where
    -- エントリーポイントとなるmain関数を検索する
    searchMain ((griffId, coreId) : _) | griffId ^. idName == "main" && griffId ^. idIsExternal = Just $ CallDirect coreId []
    searchMain (_ : xs) = searchMain xs
    searchMain _ = Nothing

-- BindGroupの脱糖衣
-- DataDef, Foreign, ScDefの順で処理する
dsBindGroup ::
  (MonadUniq m, MonadReader DsEnv m, MonadFail m, MonadIO m, MonadGriff m) =>
  BindGroup (Griff 'TypeCheck) ->
  m (DsEnv, [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))])
dsBindGroup bg = do
  env <- foldMapA dsImport (bg ^. imports)
  local (env <>) do
    (env, dataDefs') <- first mconcat <$> mapAndUnzipM dsDataDef (bg ^. dataDefs)
    local (env <>) do
      (env, foreigns') <- first mconcat <$> mapAndUnzipM dsForeign (bg ^. foreigns)
      local (env <>) do
        (env, scDefs') <- dsScDefGroup (bg ^. scDefs)
        pure $ (env,) $ mconcat $ mconcat dataDefs' <> foreigns' <> scDefs'

dsImport :: (MonadGriff m, MonadIO m) => Import (Griff 'TypeCheck) -> m DsEnv
dsImport (_, modName) = do
  interface <- loadInterface modName
  pure $ mempty & varEnv <>~ interface ^. coreIdentMap

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
        <+> quotes (pPrint name <+> ":" <+> pPrint typ)
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
  let (paramTypes, retType) = splitTyArr (x ^. toType)
  paramTypes' <- traverse dsType paramTypes
  retType <- dsType retType
  params <- traverse (newLocalId "$p") paramTypes'
  fun <- curryFun params $ C.ExtCall primName (paramTypes' :-> retType) (map C.Var params)
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
    ps <- traverse (newLocalId "$p") paramTypes'
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
    (GT.TyLazy {}, _) -> errorDoc $ "Invalid TyLazy:" <+> quotes (pPrint $ C.typeOf name')
    (_, [] :-> _) -> errorDoc $ "Invlalid type:" <+> quotes (pPrint name)
    _ -> pure ()
  if name' ^. idIsTopLevel
    then do
      -- name（name'）がトップレベルで定義されているとき、name'に対応する適切な値（クロージャ）は存在しない。
      -- そこで、name'の値が必要になったときに、都度クロージャを生成する。
      clsId <- newLocalId "$gblcls" (C.typeOf name')
      ps <- case C.typeOf name' of
        pts :-> _ -> traverse (newLocalId "$p") pts
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
      clsId <- newLocalId "$concls" (C.typeOf name')
      ps <- traverse (newLocalId "$p") pts
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
        if op' ^. idIsTopLevel
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
  ps' <- traverse (\p -> newLocalId "$p" =<< dsType (p ^. toType)) ps
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
  v <- newLocalId "$fun" =<< dsType (x ^. toType)
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
  v' <- newLocalId ("$let_" <> v ^. idName) (C.typeOf e')
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
  | all (has _VarP) ps = do
    -- 変数パターンvについて、式中に現れるすべてのvをパターンマッチ対象のuで置き換える
    let es' =
          zipWith
            ( \case
                (VarP _ v) -> local (over varEnv (Map.insert v u))
                _ -> bug Unreachable
            )
            ps
            es
    match us pss es' err
  -- Constructor Rule
  -- パターンの先頭がすべて値コンストラクタのとき
  | all (has _ConP) ps = do
    let patType = head ps ^. toType
    unless (_TyApp `has` patType || _TyCon `has` patType) $
      errorDoc $ "Not valid type:" <+> pPrint patType
    -- 型からコンストラクタの集合を求める
    let (con, ts) = splitCon patType
    conMap <- lookupConMap con ts
    -- 各コンストラクタごとにC.Caseを生成する
    cases <- for conMap $ \(conName, conType) -> do
      paramTypes <- traverse dsType $ fst $ splitTyArr conType
      let ccon = C.Con (conName ^. toText) paramTypes
      params <- traverse (newLocalId "$p") paramTypes
      -- パターン行列（未転置）
      let (pss', es') = unzip $ group conName (List.transpose (ps : pss)) es
      Unpack ccon params <$> match (params <> us) (List.transpose pss') es' err
    unfoldedType <- unfoldType patType
    pure $ Match (Cast unfoldedType $ C.Var u) $ NonEmpty.fromList cases
  -- パターンの先頭がすべてタプルのとき
  | all (has _TupleP) ps = do
    let patType = head ps ^. toType
    SumT [con@(C.Con _ ts)] <- dsType patType
    params <- traverse (newLocalId "$p") ts
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
    -- パターンの網羅性を保証するため、
    -- `_ -> err` を追加する
    hole <- newLocalId "$_" (C.typeOf u)
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
dsType (GT.TyCon con) = do
  mkcon <- kind con
  case mkcon of
    Just kcon | kcon == Star -> pure $ DataT (con ^. toText) []

              | otherwise -> errorDoc $ "Invalid kind:" <+> pPrint con <+> ":" <+> pPrint kcon
    _ -> bug Unreachable
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
unfoldType t | GT._TyApp `has` t || GT._TyCon `has` t = do
  mkt <- kind t
  case mkt of
    Just Star -> do
      let (con, ts) = splitCon t
      conMap <- lookupConMap con ts
      SumT
        <$> foldMapA
          ( \(conName, conType) ->
              Set.singleton . C.Con (conName ^. toText) <$> traverse dsType (fst $ splitTyArr conType)
          )
          conMap
    _ -> dsType t
unfoldType t = dsType t

-- Desugar Monad

lookupName :: (HasCallStack, MonadReader DsEnv m) => TcId -> m (Id C.Type)
lookupName name = do
  mname' <- view (varEnv . at name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint name)

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
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint con)

newCoreId :: MonadUniq f => Id ModuleName -> a -> f (Id a)
newCoreId griffId coreType =
  Id.newId (toCoreName griffId) coreType (griffId ^. idIsTopLevel) (griffId ^. idIsExternal)

toCoreName :: Id ModuleName -> String
toCoreName griffId = griffId ^. idMeta . _Module <> "." <> griffId ^. idName

-- 関数をカリー化する
curryFun ::
  (HasCallStack, MonadUniq m, MonadReader DsEnv m) =>
  [Id C.Type] ->
  C.Exp (Id C.Type) ->
  m ([Id C.Type], C.Exp (Id C.Type))
-- FIXME: curryFun [] e の正しい処理は、eの型に応じて引数リストpsを生成し、(ps, (apply e ps))を返す
-- そのためには、Coreの項に明示的な型の適用を追加する必要がある
curryFun [] e@(C.Let ds (Atom (C.Var v))) = case List.lookup v ds of
  Just (Fun ps e) | not $ any ((/= v) . fst) ds -> pure (ps, e)
  _ -> errorDoc $ "Invalid expression:" <+> quotes (pPrint e)
curryFun [] e = errorDoc $ "Invalid expression:" <+> quotes (pPrint e)
curryFun [x] e = pure ([x], e)
curryFun ps@(_ : _) e = curryFun' ps []
  where
    curryFun' [] _ = bug Unreachable
    curryFun' [x] as = do
      x' <- newLocalId (x ^. idName) (C.typeOf x)
      fun <- newLocalId "$curry" (C.typeOf $ Fun ps e)
      let body = C.Call (C.Var fun) $ reverse $ C.Var x' : as
      pure ([x'], C.Let [(fun, Fun ps e)] body)
    curryFun' (x : xs) as = do
      x' <- newLocalId (x ^. idName) (C.typeOf x)
      fun <- curryFun' xs (C.Var x' : as)
      let funObj = uncurry Fun fun
      body <- runDef $ do
        fun <- let_ (C.typeOf funObj) funObj
        pure $ Atom fun
      pure ([x'], body)
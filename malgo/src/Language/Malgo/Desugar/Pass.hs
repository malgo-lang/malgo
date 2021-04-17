{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | MalgoをKoriel.Coreに変換（脱糖衣）する
module Language.Malgo.Desugar.Pass (desugar) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
import Koriel.Core.Syntax as C
import Koriel.Core.Type hiding (Type)
import qualified Koriel.Core.Type as C
import Koriel.Id hiding (newGlobalId, newId)
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Desugar.DsEnv
import Language.Malgo.Interface
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import Language.Malgo.Syntax as G
import Language.Malgo.Syntax.Extension as G
import Language.Malgo.TypeRep.Static as GT

-- | MalgoからCoreへの変換
desugar :: (MonadUniq m, MonadIO m, MonadMalgo m, XModule x ~ BindGroup (Malgo 'Refine), MonadFail m) => HashMap RnId (Scheme GT.Type) -> HashMap RnTId (TypeDef GT.Type) -> RnEnv -> Module x -> m (DsEnv, Program (Id C.Type))
desugar varEnv typeEnv rnEnv (Module modName ds) = do
  (ds', dsEnv) <- runStateT (dsBindGroup ds) (makeDsEnv modName varEnv typeEnv rnEnv)
  case searchMain (HashMap.toList $ view nameEnv dsEnv) of
    Just mainCall -> do
      mainFuncDef <-
        mainFunc =<< runDef do
          _ <- bind mainCall
          pure (Atom $ C.Unboxed $ C.Int32 0)
      pure (dsEnv, Program modName (mainFuncDef : ds'))
    Nothing -> pure (dsEnv, Program modName ds')
  where
    -- エントリーポイントとなるmain関数を検索する
    searchMain ((griffId, coreId) : _) | griffId ^. idName == "main" && griffId ^. idSort == External modName = Just $ CallDirect coreId []
    searchMain (_ : xs) = searchMain xs
    searchMain _ = Nothing

-- BindGroupの脱糖衣
-- DataDef, Foreign, ScDefの順で処理する
dsBindGroup ::
  (MonadUniq m, MonadState DsEnv m, MonadIO m, MonadMalgo m, MonadFail m) =>
  BindGroup (Malgo 'Refine) ->
  m [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]
dsBindGroup bg = do
  traverse_ dsImport (bg ^. imports)
  dataDefs' <- traverse dsDataDef (bg ^. dataDefs)
  foreigns' <- traverse dsForeign (bg ^. foreigns)
  scDefs' <- dsScDefGroup (bg ^. scDefs)
  pure $ mconcat $ mconcat dataDefs' <> foreigns' <> scDefs'

dsImport :: (MonadMalgo m, MonadState DsEnv m, MonadIO m) => Import (Malgo 'Refine) -> m ()
dsImport (_, modName) = do
  interface <- loadInterface modName
  nameEnv <>= interface ^. coreIdentMap

-- 相互再帰するScDefのグループごとに脱糖衣する
dsScDefGroup ::
  (MonadUniq f, MonadState DsEnv f, MonadIO f, MonadMalgo f, MonadFail f) =>
  [[ScDef (Malgo 'Refine)]] ->
  f [[(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]]
dsScDefGroup = traverse dsScDefs

-- 相互再帰的なグループをdesugar
dsScDefs ::
  (MonadUniq f, MonadState DsEnv f, MonadIO f, MonadMalgo f, MonadFail f) =>
  [ScDef (Malgo 'Refine)] ->
  f [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]
dsScDefs ds = do
  -- まず、このグループで宣言されているScDefの名前をすべて名前環境に登録する
  for_ ds $ \(_, f, _) -> do
    Just (Forall _ fType) <- use (varTypeEnv . at f)
    f' <- newCoreId f =<< dsType fType
    nameEnv . at f ?= f'
  foldMapA dsScDef ds

dsScDef ::
  (MonadUniq f, MonadState DsEnv f, MonadIO f, MonadMalgo f, MonadFail f) =>
  ScDef (Malgo 'Refine) ->
  f [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]
dsScDef (With typ pos, name, expr) = do
  -- ScDefは関数かlazy valueでなくてはならない
  unless (_TyArr `has` typ || _TyLazy `has` typ) $
    errorOn pos $
      "Invalid Toplevel Declaration:"
        <+> quotes (pPrint name <+> ":" <+> pPrint typ)
  name' <- lookupName name
  fun <- curryFun [] =<< dsExp expr
  pure [(name', fun)]

-- TODO: Malgoのforeignでvoid型をあつかえるようにする #13
-- 1. Malgoの型とCの型の相互変換を定義する
-- 2. 相互変換を値に対して行うCoreコードを生成する関数を定義する
-- 3. 2.の関数を使ってdsForeignを書き換える
dsForeign ::
  (MonadState DsEnv f, MonadUniq f, MonadIO f) =>
  Foreign (Malgo 'Refine) ->
  f [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]
dsForeign (x@(With _ (_, primName)), name, _) = do
  name' <- newCoreId name =<< dsType (x ^. GT.withType)
  let (paramTypes, retType) = splitTyArr (x ^. GT.withType)
  paramTypes' <- traverse dsType paramTypes
  retType <- dsType retType
  params <- traverse (newLocalId "$p") paramTypes'
  fun <- curryFun params $ C.ExtCall primName (paramTypes' :-> retType) (map C.Var params)
  nameEnv . at name ?= name'
  pure [(name', fun)]

dsDataDef ::
  (MonadUniq m, MonadState DsEnv m, MonadIO m, MonadFail m) =>
  DataDef (Malgo 'Refine) ->
  m [[(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]]
dsDataDef (_, name, _, cons) =
  for cons $ \(conName, _) -> do
    -- lookup constructor infomations
    Just (GT.TyCon name') <- preuse (typeDefEnv . at name . _Just . typeConstructor)
    vcs <- lookupValueConstructors (over idMeta fromType name') []
    let Forall _ conType = fromJust $ List.lookup conName vcs

    -- desugar conType
    let (paramTypes, retType) = splitTyArr conType
    paramTypes' <- traverse dsType paramTypes
    retType' <- dsType retType

    -- generate constructor code
    conName' <- newCoreId conName $ buildConType paramTypes' retType'
    ps <- traverse (newLocalId "$p") paramTypes'
    expr <- runDef $ do
      unfoldedType <- unfoldType retType
      packed <- let_ unfoldedType (Pack unfoldedType (C.Con (Data $ conName ^. toText) paramTypes') $ map C.Var ps)
      pure $ Cast retType' packed
    obj <- case ps of
      [] -> pure ([], expr)
      _ -> curryFun ps expr
    nameEnv . at conName ?= conName'
    pure [(conName', obj)]
  where
    -- 引数のない値コンストラクタは、0引数のCore関数に変換される
    buildConType [] retType = [] :-> retType
    buildConType paramTypes retType = foldr (\a b -> [a] :-> b) retType paramTypes

-- Unboxedの脱糖衣
dsUnboxed :: Literal G.Unboxed -> C.Unboxed
dsUnboxed (G.Int32 x) = C.Int32 $ toInteger x
dsUnboxed (G.Int64 x) = C.Int64 $ toInteger x
dsUnboxed (G.Float x) = C.Float x
dsUnboxed (G.Double x) = C.Double x
dsUnboxed (G.Char x) = C.Char x
dsUnboxed (G.String x) = C.String x

dsExp ::
  (MonadUniq m, MonadState DsEnv m, MonadIO m, MonadFail m) =>
  G.Exp (Malgo 'Refine) ->
  m (C.Exp (Id C.Type))
dsExp (G.Var x name) = do
  name' <- lookupName name
  -- Malgoでの型とCoreでの型に矛盾がないかを検査
  -- Note: [0 argument]
  --   Core上で0引数関数で表現されるMalgoの値は以下の二つ。
  --    1. {a}型の値（TyLazy）
  --    2. 引数のない値コンストラクタ
  --   このうち、2.は「0引数関数の呼び出し」の形でのみ出現する（dsExp G.Conの節参照）
  --   よって、ここではxがTyLazyのときのみname'が0引数関数になるはずである。
  case (x ^. GT.withType, C.typeOf name') of
    -- TyLazyの型を検査
    (GT.TyLazy {}, [] :-> _) -> pure ()
    (GT.TyLazy {}, _) -> errorDoc $ "Invalid TyLazy:" <+> quotes (pPrint $ C.typeOf name')
    (_, [] :-> _) -> errorDoc $ "Invlalid type:" <+> quotes (pPrint name)
    _ -> pure ()
  if idIsExternal name'
    then do
      -- name（name'）がトップレベルで定義されているとき、name'に対応する適切な値（クロージャ）は存在しない。
      -- そこで、name'の値が必要になったときに、都度クロージャを生成する。
      clsId <- newLocalId "$gblcls" (C.typeOf name')
      ps <- case C.typeOf name' of
        pts :-> _ -> traverse (newLocalId "$p") pts
        _ -> bug Unreachable
      pure $ C.Let [LocalDef clsId (Fun ps $ CallDirect name' $ map C.Var ps)] $ Atom $ C.Var clsId
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
      pure $ C.Let [C.LocalDef clsId (Fun ps $ CallDirect name' $ map C.Var ps)] $ Atom $ C.Var clsId
    _ -> bug Unreachable
dsExp (G.Unboxed _ u) = pure $ Atom $ C.Unboxed $ dsUnboxed u
dsExp (G.Apply info f x) = runDef $ do
  f' <- bind =<< dsExp f
  case C.typeOf f' of
    [xType] :-> _ -> do
      -- Note: [Cast Argument Type]
      --   x の型と f の引数の型は必ずしも一致しない
      --   適切な型にcastする必要がある
      x' <- cast xType =<< dsExp x
      Cast <$> dsType (info ^. GT.withType) <*> bind (Call f' [x'])
    _ -> bug Unreachable
dsExp (G.Fn x (Clause _ [] ss : _)) = do
  -- lazy valueの脱糖衣
  ss' <- dsStmts ss
  typ <- dsType (x ^. GT.withType)
  runDef do
    fun <- let_ typ $ Fun [] ss'
    pure $ Atom fun
dsExp (G.Fn x cs@(Clause _ ps es : _)) = do
  ps' <- traverse (\p -> newLocalId "$p" =<< dsType =<< GT.typeOf p) ps
  typ <- dsType =<< GT.typeOf (last es)
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
  v <- newLocalId "$fun" =<< dsType (x ^. GT.withType)
  pure $ C.Let [C.LocalDef v (uncurry Fun obj)] $ Atom $ C.Var v
dsExp (G.Fn _ []) = bug Unreachable
dsExp (G.Tuple _ es) = runDef $ do
  es' <- traverse (bind <=< dsExp) es
  let con = C.Con C.Tuple $ map C.typeOf es'
  let ty = SumT [con]
  tuple <- let_ ty $ Pack ty con es'
  pure $ Atom tuple
dsExp (G.Force _ e) = runDef $ do
  -- lazy valueは0引数関数に変換されるので、その評価は0引数関数の呼び出しになる
  e' <- bind =<< dsExp e
  pure $ Call e' []
dsExp (G.Parens _ e) = dsExp e

dsStmts :: (MonadUniq m, MonadState DsEnv m, MonadIO m, MonadFail m) => [Stmt (Malgo 'Refine)] -> m (C.Exp (Id C.Type))
dsStmts [] = bug Unreachable
dsStmts [NoBind _ e] = dsExp e
dsStmts [G.Let _ _ e] = dsExp e
dsStmts (NoBind _ e : ss) = runDef $ do
  _ <- bind =<< dsExp e
  dsStmts ss
dsStmts (G.Let _ v e : ss) = do
  e' <- dsExp e
  v' <- newLocalId ("$let_" <> v ^. idName) (C.typeOf e')
  nameEnv . at v ?= v'
  ss' <- dsStmts ss
  pure $ Match e' (Bind v' ss' :| [])

-- TODO: The Implementation of Functional Programming Languages
-- を元にコメントを追加

-- パターンマッチを分解し、switch-case相当の分岐で表現できるように変換する
match ::
  (MonadState DsEnv m, MonadIO m, MonadUniq m, MonadFail m) =>
  [Id C.Type] ->
  [[Pat (Malgo 'Refine)]] ->
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
                (VarP _ v) -> \e -> nameEnv . at v ?= u >> e
                _ -> bug Unreachable
            )
            ps
            es
    match us pss es' err
  -- Constructor Rule
  -- パターンの先頭がすべて値コンストラクタのとき
  | all (has _ConP) ps = do
    patType <- GT.typeOf $ head ps
    unless (_TyApp `has` patType || _TyCon `has` patType) $
      errorDoc $ "Not valid type:" <+> pPrint patType
    -- 型からコンストラクタの集合を求める
    let (con, ts) = splitCon patType
    vcs <- lookupValueConstructors con ts
    -- 各コンストラクタごとにC.Caseを生成する
    cases <- for vcs $ \(conName, Forall _ conType) -> do
      paramTypes <- traverse dsType $ fst $ splitTyArr conType
      let ccon = C.Con (Data $ conName ^. toText) paramTypes
      params <- traverse (newLocalId "$p") paramTypes
      -- パターン行列（未転置）
      let (pss', es') = unzip $ group conName (List.transpose (ps : pss)) es
      Unpack ccon params <$> match (params <> us) (List.transpose pss') es' err
    unfoldedType <- unfoldType patType
    pure $ Match (Cast unfoldedType $ C.Var u) $ NonEmpty.fromList cases
  -- パターンの先頭がすべてタプルのとき
  | all (has _TupleP) ps = do
    patType <- GT.typeOf $ head ps
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

-- Malgoの型をCoreの型に変換する
dsType :: Monad m => GT.Type -> m C.Type
dsType GT.TyApp {} = pure AnyT
dsType (GT.TyVar _) = pure AnyT
dsType (GT.TyCon con) = do
  case con ^. idMeta of
    TYPE (Rep BoxedRep) -> pure AnyT
    kcon -> errorDoc $ "Invalid kind:" <+> pPrint con <+> ":" <+> pPrint kcon
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
  SumT . pure . C.Con C.Tuple <$> traverse dsType ts
dsType (GT.TyLazy t) = ([] :->) <$> dsType t
dsType (GT.TyPtr t) = PtrT <$> dsType t
dsType t = errorDoc $ "invalid type on dsType:" <+> pPrint t

-- List aのような型を、<Nil | Cons a (List a)>のような和型に展開する
unfoldType :: (MonadState DsEnv m, MonadIO m) => GT.Type -> m C.Type
unfoldType t | GT._TyApp `has` t || GT._TyCon `has` t = do
  GT.kindOf t >>= \case
    TYPE (Rep BoxedRep) -> do
      let (con, ts) = splitCon t
      vcs <- lookupValueConstructors con ts
      SumT
        <$> traverse
          ( \(conName, Forall _ conType) ->
              C.Con (Data $ conName ^. toText) <$> traverse dsType (fst $ splitTyArr conType)
          )
          vcs
    _ -> dsType t
unfoldType t = dsType t

-- Desugar Monad

lookupName :: MonadState DsEnv m => RnId -> m (Id C.Type)
lookupName name = do
  mname' <- use (nameEnv . at name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint name)

lookupValueConstructors ::
  MonadState DsEnv m =>
  Id GT.Type ->
  [GT.Type] ->
  m [(RnId, Scheme GT.Type)]
lookupValueConstructors con ts = do
  typeEnv <- use typeDefEnv
  case List.find (\TypeDef {..} -> _typeConstructor == GT.TyCon con) (HashMap.elems typeEnv) of
    Just TypeDef {..} ->
      pure $ over (mapped . _2 . traversed) (GT.applySubst $ HashMap.fromList $ zip _typeParameters ts) _valueConstructors
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint con)

newCoreId :: MonadUniq f => RnId -> C.Type -> f (Id C.Type)
newCoreId griffId coreType = newIdOnName coreType griffId

-- 関数をカリー化する
curryFun ::
  (MonadUniq m, MonadState DsEnv m) =>
  [Id C.Type] ->
  C.Exp (Id C.Type) ->
  m ([Id C.Type], C.Exp (Id C.Type))
-- FIXME: curryFun [] e の正しい処理は、eの型に応じて引数リストpsを生成し、(ps, (apply e ps))を返す
-- そのためには、Coreの項に明示的な型の適用を追加する必要がある
curryFun [] (C.Let [LocalDef v (Fun ps e)] (Atom (C.Var v'))) | v == v' = pure (ps, e)
curryFun [] e = errorDoc $ "Invalid expression:" <+> quotes (pPrint e)
curryFun [x] e = pure ([x], e)
curryFun ps@(_ : _) e = curryFun' ps []
  where
    curryFun' [] _ = bug Unreachable
    curryFun' [x] as = do
      x' <- newLocalId (x ^. idName) (C.typeOf x)
      fun <- newLocalId "$curry" (C.typeOf $ Fun ps e)
      let body = C.Call (C.Var fun) $ reverse $ C.Var x' : as
      pure ([x'], C.Let [C.LocalDef fun $ Fun ps e] body)
    curryFun' (x : xs) as = do
      x' <- newLocalId (x ^. idName) (C.typeOf x)
      fun <- curryFun' xs (C.Var x' : as)
      let funObj = uncurry Fun fun
      body <- runDef $ do
        fun <- let_ (C.typeOf funObj) funObj
        pure $ Atom fun
      pure ([x'], body)
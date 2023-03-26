-- | MalgoをKoriel.Coreに変換（脱糖衣）する
module Malgo.Desugar.Pass (desugar) where

import Control.Lens (At (at), preuse, preview, traverseOf, traversed, use, (<>=), (?=), (^.), _2, _Just)
import Data.Char qualified as Char
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Traversable (for)
import Koriel.Core.Syntax as C
import Koriel.Core.Type hiding (Type)
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Desugar.DsEnv
import Malgo.Desugar.DsState
import Malgo.Desugar.Match
import Malgo.Desugar.Type
import Malgo.Infer.TcEnv (TcEnv)
import Malgo.Infer.TypeRep as GT
import Malgo.Interface
import Malgo.Monad
import Malgo.Prelude
import Malgo.Syntax as G
import Malgo.Syntax.Extension as G

-- | MalgoからCoreへの変換
desugar ::
  (MonadReader MalgoEnv m, MonadFail m, MonadIO m) =>
  TcEnv ->
  Module (Malgo 'Refine) ->
  m (DsState, Program (Id C.Type))
desugar tcEnv (Module _ ds) = do
  malgoEnv <- ask
  runReaderT ?? makeDsEnv malgoEnv $ do
    (ds', dsEnv) <- runStateT (dsBindGroup ds) (makeDsState tcEnv)
    let ds'' = ds' <> dsEnv._globalDefs
    let varDefs = mapMaybe (preview _VarDef) ds''
    let funDefs = mapMaybe (preview _FunDef) ds''
    let extDefs = mapMaybe (preview _ExtDef) ds''
    pure (dsEnv, Program varDefs funDefs extDefs)

-- BindGroupの脱糖衣
-- DataDef, Foreign, ScDefの順で処理する
dsBindGroup ::
  (MonadState DsState m, MonadReader DsEnv m, MonadFail m, MonadIO m) =>
  BindGroup (Malgo 'Refine) ->
  m [Def]
dsBindGroup bg = do
  traverse_ dsImport (bg ^. imports)
  dataDefs' <- traverse dsDataDef (bg ^. dataDefs)
  foreigns' <- traverse dsForeign (bg ^. foreigns)
  scDefs' <- dsScDefGroup (bg ^. scDefs)
  pure $ mconcat dataDefs' <> mconcat foreigns' <> scDefs'

dsImport :: (MonadReader DsEnv m, MonadState DsState m, MonadIO m) => Import (Malgo 'Refine) -> m ()
dsImport (_, modName, _) = do
  interface <- loadInterface modName
  nameEnv <>= interface ^. coreIdentMap

-- ScDefのグループを一つのリストにつぶしてから脱糖衣する
dsScDefGroup ::
  (MonadState DsState f, MonadReader DsEnv f, MonadFail f, MonadIO f) =>
  [[ScDef (Malgo 'Refine)]] ->
  f [Def]
dsScDefGroup = dsScDefs . mconcat

dsScDefs ::
  (MonadState DsState f, MonadReader DsEnv f, MonadFail f, MonadIO f) =>
  [ScDef (Malgo 'Refine)] ->
  f [Def]
dsScDefs ds = do
  -- まず、宣言されているScDefの名前をすべて名前環境に登録する
  for_ ds $ \(_, f, _) -> do
    Just (Forall _ fType) <- use (signatureMap . at f)
    f' <- toCoreId f <$> dsType fType
    nameEnv . at f ?= f'
  foldMapM dsScDef ds

dsScDef :: (MonadState DsState f, MonadReader DsEnv f, MonadFail f, MonadIO f) => ScDef (Malgo 'Refine) -> f [Def]
dsScDef (Typed typ _, name, expr) = do
  -- ScDefは関数かlazy valueでなくてはならない
  case typ of
    GT.TyArr _ _ -> dsFunDef name expr
    _ -> dsVarDef name expr
  where
    dsVarDef name expr = do
      name' <- lookupName name
      typ' <- dsType typ
      expr' <- runDef $ fmap Atom $ cast typ' =<< dsExp expr
      pure [VarDef name' typ' expr']
    dsFunDef name (G.Fn _ cs) = do
      name' <- lookupName name
      (ps, e) <- fnToObj True name.name cs
      pure [FunDef name' ps (C.typeOf name') e]
    dsFunDef name expr = do
      name' <- lookupName name
      typ' <- dsType typ
      (ps, e) <- curryFun True name.name [] =<< runDef (fmap Atom (cast typ' =<< dsExp expr))
      pure [FunDef name' ps (C.typeOf name') e]

fnToObj :: (MonadIO f, MonadFail f, MonadReader DsEnv f, MonadState DsState f) => Bool -> Text -> NonEmpty (Clause (Malgo 'Refine)) -> f ([Id C.Type], C.Exp (Id C.Type))
fnToObj isToplevel hint cs@(Clause _ ps e :| _) = do
  ps' <- traverse (\p -> newTemporalId (patToName p) =<< dsType (GT.typeOf p)) ps
  eType <- dsType (GT.typeOf e)
  -- destruct Clauses
  (pss, es) <-
    unzip
      <$> traverse
        ( \(Clause _ ps e) ->
            pure (ps, dsExp e)
        )
        cs
  body <- match ps' (patMatrix $ toList pss) (toList es) (Error eType)
  curryFun isToplevel hint ps' body

patToName :: Pat (Malgo 'Refine) -> Text
patToName (G.VarP _ v) = v.name
patToName (G.ConP _ c _) = T.toLower $ c.name
patToName (G.TupleP _ _) = "tuple"
patToName (G.RecordP _ _) = "record"
patToName (G.UnboxedP _ _) = "unboxed"

-- TODO: Malgoのforeignでvoid型をあつかえるようにする #13
-- 1. Malgoの型とCの型の相互変換を定義する
-- 2. 相互変換を値に対して行うCoreコードを生成する関数を定義する
-- 3. 2.の関数を使ってdsForeignを書き換える
dsForeign ::
  (MonadState DsState f, MonadIO f, MonadReader DsEnv f) =>
  Foreign (Malgo 'Refine) ->
  f [Def]
dsForeign (Typed typ (_, primName), name, _) = do
  name' <- toCoreId name <$> dsType typ
  let (paramTypes, retType) = splitTyArr typ
  paramTypes' <- traverse dsType paramTypes
  retType <- dsType retType
  params <- traverse (newTemporalId "p") paramTypes'
  (ps, e) <- curryFun True name.name params $ C.RawCall primName (paramTypes' :-> retType) (map C.Var params)
  nameEnv . at name ?= name'
  pure [FunDef name' ps (C.typeOf name') e, ExtDef primName (paramTypes' :-> retType)]

dsDataDef ::
  (MonadState DsState m, MonadReader DsEnv m, MonadFail m, MonadIO m) =>
  DataDef (Malgo 'Refine) ->
  m [Def]
dsDataDef (_, name, _, cons) =
  for cons $ \(_, conName, _) -> do
    -- lookup constructor infomations
    Just vcs <- preuse (typeDefMap . at name . _Just . valueConstructors)
    let Forall _ conType = fromJust $ List.lookup conName vcs

    -- desugar conType
    let (paramTypes, retType) = splitTyArr conType
    paramTypes' <- traverse dsType paramTypes
    retType' <- dsType retType

    -- generate constructor code
    let conName' = toCoreId conName $ buildConType paramTypes' retType'
    ps <- traverse (newTemporalId "p") paramTypes'
    expr <- runDef $ do
      unfoldedType <- unfoldType retType
      packed <- let_ unfoldedType (Pack unfoldedType (C.Con (Data $ idToText conName) paramTypes') $ map C.Var ps)
      pure $ Cast retType' packed
    (ps, e) <- case ps of
      [] -> pure ([], expr)
      _ -> curryFun True conName.name ps expr
    nameEnv . at conName ?= conName'
    pure (FunDef conName' ps (C.typeOf conName') e)
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
  (MonadState DsState m, MonadIO m, MonadFail m, MonadReader DsEnv m) =>
  G.Exp (Malgo 'Refine) ->
  m (C.Exp (Id C.Type))
dsExp (G.Var (Typed typ _) name) = do
  name' <- lookupName name
  -- Malgoでの型とCoreでの型に矛盾がないかを検査
  -- 引数のない値コンストラクタは、Coreでは0引数の関数として扱われる
  case (typ, C.typeOf name') of
    (_, [] :-> _)
      | isConstructor name -> pass
      | otherwise -> errorDoc $ "Invalid type:" <+> quotes (pPrint name)
    _ -> pass
  case C.typeOf name' of
    -- 引数のない値コンストラクタは、0引数関数の呼び出しに変換する（クロージャは作らない）
    [] :-> _ | isConstructor name -> pure $ CallDirect name' []
    _
      | idIsExternal name' -> do
          -- name（name'）がトップレベルで定義されているとき、name'に対応する適切な値（クロージャ）は存在しない。
          -- そこで、name'の値が必要になったときに、都度クロージャを生成する。
          case C.typeOf name' of
            pts :-> _ -> do
              clsId <- newTemporalId "gblcls" (C.typeOf name')
              ps <- traverse (newTemporalId "p") pts
              pure $ C.Let [LocalDef clsId (C.typeOf clsId) (Fun ps $ CallDirect name' $ map C.Var ps)] $ Atom $ C.Var clsId
            _ -> pure $ Atom $ C.Var name'
      | otherwise -> pure $ Atom $ C.Var name'
  where
    isConstructor Id {name} | T.length name > 0 = Char.isUpper (T.head name)
    isConstructor _ = False
dsExp (G.Unboxed _ u) = pure $ Atom $ C.Unboxed $ dsUnboxed u
dsExp (G.Apply (Typed typ _) fun arg) = runDef $ do
  fun' <- bind =<< dsExp fun
  case C.typeOf fun' of
    [paramType] :-> _ -> do
      -- Note: [Cast Argument Type]
      --   x の型と f の引数の型は必ずしも一致しない
      --   適切な型にcastする必要がある
      arg' <- cast paramType =<< dsExp arg
      Cast <$> dsType typ <*> bind (Call fun' [arg'])
    _ ->
      error "typeOf f' must be [_] :-> _. All functions which evaluated by Apply are single-parameter function"
dsExp (G.Fn (Typed typ _) cs) = do
  obj <- fnToObj False "inner" cs
  v <- newTemporalId "fun" =<< dsType typ
  pure $ C.Let [C.LocalDef v (C.typeOf v) (uncurry Fun obj)] $ Atom $ C.Var v
dsExp (G.Tuple _ es) = runDef $ do
  es' <- traverse (bind <=< dsExp) es
  let con = C.Con C.Tuple $ map C.typeOf es'
  let ty = SumT [con]
  tuple <- let_ ty $ Pack ty con es'
  pure $ Atom tuple
dsExp (G.Record (Typed (GT.TyRecord recordType) _) kvs) = runDef $ do
  kvs' <- traverseOf (traversed . _2) (bind <=< dsExp) kvs
  kts <- HashMap.toList <$> traverse dsType recordType
  v <- newTemporalId "record" $ RecordT (HashMap.fromList kts)
  pure $ C.Let [C.LocalDef v (C.typeOf v) (C.Record $ HashMap.fromList kvs')] $ Atom $ C.Var v
dsExp (G.Record _ _) = error "unreachable"
dsExp (G.Seq _ ss) = dsStmts ss

dsStmts :: (MonadState DsState m, MonadIO m, MonadFail m, MonadReader DsEnv m) => NonEmpty (Stmt (Malgo 'Refine)) -> m (C.Exp (Id C.Type))
dsStmts (NoBind _ e :| []) = dsExp e
dsStmts (G.Let _ _ e :| []) = dsExp e
dsStmts (NoBind _ e :| s : ss) = runDef $ do
  _ <- bind =<< dsExp e
  dsStmts (s :| ss)
dsStmts (G.Let _ v e :| s : ss) = do
  e' <- dsExp e
  v' <- newTemporalId ("let_" <> idToText v) (C.typeOf e')
  nameEnv . at v ?= v'
  ss' <- dsStmts (s :| ss)
  pure $ Match e' [Bind v' (C.typeOf v') ss']

-- Desugar Monad

lookupName :: MonadState DsState m => RnId -> m (Id C.Type)
lookupName name = do
  mname' <- use (nameEnv . at name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint name)

toCoreId :: RnId -> C.Type -> Id C.Type
toCoreId griffId coreType = griffId {meta = coreType}

-- 関数をカリー化する
curryFun ::
  (MonadIO m, MonadReader DsEnv m, MonadState DsState m) =>
  -- | トップレベル関数か否か
  Bool ->
  -- | uncurryされた関数名のヒント
  Text ->
  -- | パラメータリスト
  [Id C.Type] ->
  -- | uncurryされた関数値
  C.Exp (Id C.Type) ->
  m ([Id C.Type], C.Exp (Id C.Type))
-- η展開
curryFun isToplevel hint [] e = do
  case C.typeOf e of
    [] :-> _ -> do
      body <- runDef do
        f <- bind e
        pure $ C.Call f []
      pure ([], body)
    pts :-> _ -> do
      ps <- traverse (newTemporalId "eta") pts
      body <- runDef do
        f <- bind e
        pure $ C.Call f (map C.Var ps)
      curryFun isToplevel hint ps body
    _ -> errorDoc $ "Invalid expression:" <+> quotes (pPrint e)
curryFun isToplevel hint ps e = curryFun' ps []
  where
    curryFun' [] _ = error "length ps >= 1"
    curryFun' [x] as = do
      if isToplevel
        then do
          -- トップレベル関数であるならeに自由変数は含まれないので、
          -- uncurry後の関数もトップレベル関数にできる。
          fun <- newExternalId (hint <> "_curry") (C.typeOf $ Fun ps e) -- =<< view moduleName
          globalDefs <>= [FunDef fun ps (C.typeOf fun) e]
          let body = C.CallDirect fun $ reverse $ C.Var x : as
          pure ([x], body)
        else do
          fun <- newTemporalId (hint <> "_curry") (C.typeOf $ Fun ps e) -- =<< view moduleName
          let body = C.Call (C.Var fun) $ reverse $ C.Var x : as
          pure ([x], C.Let [LocalDef fun (C.typeOf fun) (Fun ps e)] body)
    curryFun' (x : xs) as = do
      fun <- curryFun' xs (C.Var x : as)
      let funObj = uncurry Fun fun
      body <- runDef $ do
        fun <- let_ (C.typeOf funObj) funObj
        pure $ Atom fun
      pure ([x], body)

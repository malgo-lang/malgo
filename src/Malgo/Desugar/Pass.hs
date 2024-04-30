-- | MalgoをMalgo.Coreに変換（脱糖衣）する
module Malgo.Desugar.Pass (desugar) where

import Control.Lens (preview, traverseOf, traversed, (^.), _2)
import Data.Char qualified as Char
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Malgo.Core.Alpha (alpha)
import Malgo.Core.Syntax as C
import Malgo.Core.Type hiding (Type)
import Malgo.Core.Type qualified as C
import Malgo.Desugar.DsState
import Malgo.Desugar.Match
import Malgo.Desugar.Type
import Malgo.Id
import Malgo.Infer.TcEnv (TcEnv)
import Malgo.Infer.TypeRep as GT
import Malgo.Interface
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.Syntax as G
import Malgo.Syntax.Extension as G

-- | MalgoからCoreへの変換
desugar ::
  (IOE :> es, State (HashMap ModuleName Interface) :> es, State Uniq :> es, Workspace :> es) =>
  TcEnv ->
  Module (Malgo 'Refine) ->
  Eff es (DsState, Program (Meta C.Type))
desugar tcEnv (Module name ds) = runReader name do
  (ds', dsEnv) <- runState (makeDsState tcEnv) (dsBindGroup ds)
  let ds'' = dsEnv._globalDefs <> ds' -- ds' needs variables defined in globalDefs
  let varDefs = mapMaybe (preview _VarDef) ds''
  let funDefs = mapMaybe (preview _FunDef) ds''
  let extDefs = mapMaybe (preview _ExtDef) ds''
  pure (dsEnv, Program varDefs funDefs extDefs)

-- BindGroupの脱糖衣
-- DataDef, Foreign, ScDefの順で処理する
dsBindGroup :: (State (HashMap ModuleName Interface) :> es, State DsState :> es, IOE :> es, Reader ModuleName :> es, State Uniq :> es, Workspace :> es) => BindGroup (Malgo Refine) -> Eff es [Def]
dsBindGroup bg = do
  traverse_ dsImport (bg ^. imports)
  dataDefs' <- traverse dsDataDef (bg ^. dataDefs)
  foreigns' <- traverse dsForeign (bg ^. foreigns)
  scDefs' <- dsScDefGroup (bg ^. scDefs)
  pure $ mconcat dataDefs' <> mconcat foreigns' <> scDefs'

dsImport ::
  ( State (HashMap ModuleName Interface) :> es,
    State DsState :> es,
    IOE :> es,
    Workspace :> es
  ) =>
  Import (Malgo Refine) ->
  Eff es ()
dsImport (_, modName, _) = do
  interface <- loadInterface modName
  modify \s@DsState {..} -> s {_nameEnv = HashMap.mapKeys (externalFromInterface interface) interface.coreIdentMap <> _nameEnv}

-- ScDefのグループを一つのリストにつぶしてから脱糖衣する
dsScDefGroup ::
  ( Reader ModuleName :> es,
    State DsState :> es,
    State Uniq :> es
  ) =>
  [[ScDef (Malgo Refine)]] ->
  Eff es [Def]
dsScDefGroup xs = dsScDefs $ mconcat xs

dsScDefs ::
  ( State DsState :> es,
    Reader ModuleName :> es,
    State Uniq :> es
  ) =>
  [ScDef (Malgo Refine)] ->
  Eff es [Def]
dsScDefs ds = do
  -- まず、宣言されているScDefの名前をすべて名前環境に登録する
  for_ ds $ \(_, f, _) -> do
    Forall _ fType <- gets @DsState ((._signatureMap) >>> HashMap.lookup f >>> fromJust)
    f' <- toCoreId f <$> dsType fType
    modify \s@DsState {..} -> s {_nameEnv = HashMap.insert f f' _nameEnv}
  foldMapM dsScDef ds

dsScDef ::
  ( Reader ModuleName :> es,
    State Uniq :> es,
    State DsState :> es
  ) =>
  ScDef (Malgo Refine) ->
  Eff es [Def]
dsScDef (Typed typ _, name, expr) = do
  -- ScDefは関数かlazy valueでなくてはならない
  case typ of
    GT.TyArr _ _ -> dsFunDef name expr
    _ -> dsVarDef name expr
  where
    dsVarDef name expr = do
      name' <- lookupName name
      typ' <- dsType typ
      expr' <- runDef $ fmap Atom $ cast typ' =<< dsExpr expr
      pure [VarDef name' typ' expr']
    dsFunDef name (G.Fn _ cs) = do
      name' <- lookupName name
      (ps, e) <- fnToObj True name.name cs
      pure [FunDef name' ps (C.typeOf name') e]
    dsFunDef name expr = do
      name' <- lookupName name
      typ' <- dsType typ
      (ps, e) <- curryFun True name.name [] =<< runDef (fmap Atom (cast typ' =<< dsExpr expr))
      pure [FunDef name' ps (C.typeOf name') e]

fnToObj ::
  ( State Uniq :> es,
    Reader ModuleName :> es,
    State DsState :> es
  ) =>
  Bool ->
  Text ->
  NonEmpty (Clause (Malgo Refine)) ->
  Eff es ([Meta C.Type], C.Expr (Meta C.Type))
fnToObj isToplevel hint cs@(Clause _ ps e :| _) = do
  ps' <- traverse (\p -> withMeta <$> dsType (GT.typeOf p) <*> newTemporalId (patToName p)) ps
  eType <- dsType (GT.typeOf e)
  -- destruct Clauses
  (pss, es) <-
    unzip
      <$> traverse
        ( \(Clause _ ps e) ->
            pure (ps, dsExpr e)
        )
        cs
  body <- match ps' (patMatrix $ toList pss) (toList es) (Error eType)
  curryFun isToplevel hint ps' body

patToName :: Pat (Malgo 'Refine) -> Text
patToName (G.VarP _ v) = v.name
patToName (G.ConP _ c _) = T.toLower c.name
patToName (G.TupleP _ _) = "tuple"
patToName (G.RecordP _ _) = "record"
patToName (G.UnboxedP _ _) = "unboxed"

-- TODO: Malgoのforeignでvoid型をあつかえるようにする #13
-- 1. Malgoの型とCの型の相互変換を定義する
-- 2. 相互変換を値に対して行うCoreコードを生成する関数を定義する
-- 3. 2.の関数を使ってdsForeignを書き換える
dsForeign ::
  (Reader ModuleName :> es, State Uniq :> es, State DsState :> es) =>
  Foreign (Malgo Refine) ->
  Eff es [Def]
dsForeign (Typed typ (_, primName), name, _) = do
  name' <- toCoreId name <$> dsType typ
  let (paramTypes, retType) = splitTyArr typ
  paramTypes' <- traverse dsType paramTypes
  retType <- dsType retType
  params <- traverse (\t -> withMeta t <$> newTemporalId "p") paramTypes'
  (ps, e) <- curryFun True name.name params $ C.RawCall primName (paramTypes' :-> retType) (map C.Var params)
  modify \s@DsState {..} -> s {_nameEnv = HashMap.insert name name' _nameEnv}
  pure [FunDef name' ps (C.typeOf name') e, ExtDef primName (paramTypes' :-> retType)]

dsDataDef :: (State DsState :> es, Reader ModuleName :> es, State Uniq :> es) => DataDef (Malgo 'Refine) -> Eff es [Def]
dsDataDef (_, name, _, cons) =
  for cons $ \(_, conName, _) -> do
    -- lookup constructor infomations
    vcs <- ((._valueConstructors) . fromJust) . HashMap.lookup name <$> gets @DsState (._typeDefMap)
    let Forall _ conType = fromJust $ List.lookup conName vcs

    -- desugar conType
    let (paramTypes, retType) = splitTyArr conType
    paramTypes' <- traverse dsType paramTypes
    retType' <- dsType retType

    -- generate constructor code
    let conName' = toCoreId conName $ buildConType paramTypes' retType'
    ps <- traverse (\t -> withMeta t <$> newTemporalId "p") paramTypes'
    expr <- runDef $ do
      unfoldedType <- unfoldType retType
      packed <- let_ unfoldedType (Pack unfoldedType (C.Con (Data $ idToText conName) paramTypes') $ map C.Var ps)
      pure $ Cast retType' packed
    (ps, e) <- case ps of
      [] -> pure ([], expr)
      _ -> curryFun True conName.name ps expr
    modify \s@DsState {..} -> s {_nameEnv = HashMap.insert conName conName' _nameEnv}
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

dsExpr :: (State DsState :> es, State Uniq :> es, Reader ModuleName :> es) => G.Expr (Malgo Refine) -> Eff es (C.Expr (Meta C.Type))
dsExpr (G.Var (Typed typ _) name) = do
  name' <- lookupName name
  -- Malgoでの型とCoreでの型に矛盾がないかを検査
  -- 引数のない値コンストラクタは、Coreでは0引数の関数として扱われる
  case (typ, C.typeOf name') of
    (_, [] :-> _)
      | isConstructor name -> pass
      | otherwise -> errorDoc $ "Invalid type:" <+> squotes (pretty name)
    _ -> pass
  case C.typeOf name' of
    -- 引数のない値コンストラクタは、0引数関数の呼び出しに変換する（クロージャは作らない）
    [] :-> _ | isConstructor name -> pure $ CallDirect name' []
    _ -> pure $ Atom $ C.Var name'
  where
    isConstructor Id {name} | T.length name > 0 = Char.isUpper (T.head name)
    isConstructor _ = False
dsExpr (G.Unboxed _ u) = pure $ Atom $ C.Unboxed $ dsUnboxed u
dsExpr (G.Apply (Typed typ _) fun arg) = runDef $ do
  fun' <- bind =<< dsExpr fun
  case C.typeOf fun' of
    [paramType] :-> _ -> do
      -- Note: [Cast Argument Type]
      --   x の型と f の引数の型は必ずしも一致しない
      --   適切な型にcastする必要がある
      arg' <- cast paramType =<< dsExpr arg
      Cast <$> dsType typ <*> bind (Call fun' [arg'])
    _ ->
      error "typeOf f' must be [_] :-> _. All functions which evaluated by Apply are single-parameter function"
dsExpr (G.Fn (Typed typ _) cs) = do
  obj <- fnToObj False "inner" cs
  v <- withMeta <$> dsType typ <*> newTemporalId "fun"
  pure $ C.Let [C.LocalDef v (C.typeOf v) (uncurry Fun obj)] $ Atom $ C.Var v
dsExpr (G.Tuple _ es) = runDef $ do
  es' <- traverse (bind <=< dsExpr) es
  let con = C.Con C.Tuple $ map C.typeOf es'
  let ty = SumT [con]
  tuple <- let_ ty $ Pack ty con es'
  pure $ Atom tuple
dsExpr (G.Record (Typed (GT.TyRecord recordType) _) kvs) = runDef $ do
  kvs' <- traverseOf (traversed . _2) (bind <=< dsExpr) kvs
  kts <- HashMap.toList <$> traverse dsType recordType
  v <- withMeta (RecordT (HashMap.fromList kts)) <$> newTemporalId "record"
  pure $ C.Let [C.LocalDef v (C.typeOf v) (C.Record $ HashMap.fromList kvs')] $ Atom $ C.Var v
dsExpr (G.Record _ _) = error "unreachable"
dsExpr (G.Seq _ ss) = dsStmts ss

dsStmts :: (State Uniq :> es, Reader ModuleName :> es, State DsState :> es) => NonEmpty (Stmt (Malgo Refine)) -> Eff es (C.Expr (Meta C.Type))
dsStmts (NoBind _ e :| []) = dsExpr e
dsStmts (G.Let _ _ e :| []) = dsExpr e
dsStmts (NoBind _ e :| s : ss) = runDef $ do
  _ <- bind =<< dsExpr e
  dsStmts (s :| ss)
dsStmts (G.Let _ v e :| s : ss) = do
  e' <- dsExpr e
  v' <- withMeta (C.typeOf e') <$> newTemporalId ("let_" <> idToText v)
  modify $ \s@DsState {..} -> s {_nameEnv = HashMap.insert v v' _nameEnv}
  ss' <- dsStmts (s :| ss)
  pure $ Match e' [Bind v' (C.typeOf v') ss']

-- Desugar Monad

lookupName :: (State DsState :> es) => Id -> Eff es (Meta C.Type)
lookupName name = do
  mname' <- gets @DsState ((._nameEnv) >>> HashMap.lookup name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> squotes (pretty name)

toCoreId :: RnId -> C.Type -> Meta C.Type
toCoreId griffId coreType = Meta {meta = coreType, id = griffId}

-- 関数をカリー化する
-- η展開
curryFun ::
  ( State Uniq :> es,
    Reader ModuleName :> es,
    State DsState :> es
  ) =>
  Bool ->
  Text ->
  [Meta C.Type] ->
  C.Expr (Meta C.Type) ->
  Eff es ([Meta C.Type], C.Expr (Meta C.Type))
curryFun isToplevel hint [] e = do
  case C.typeOf e of
    [] :-> _ -> do
      body <- runDef do
        f <- bind e
        pure $ C.Call f []
      pure ([], body)
    pts :-> _ -> do
      ps <- traverse (\t -> withMeta t <$> newTemporalId "eta") pts
      body <- runDef do
        f <- bind e
        pure $ C.Call f (map C.Var ps)
      curryFun isToplevel hint ps body
    _ -> errorDoc $ "Invalid expression:" <+> squotes (pretty e)
curryFun _ _ [p] e = pure ([p], e)
curryFun isToplevel hint ps e = curryFun' ps []
  where
    curryFun' [] _ = error "length ps >= 1"
    curryFun' [x] as = do
      if isToplevel
        then do
          -- トップレベル関数であるならeに自由変数は含まれないので、
          -- uncurry後の関数もトップレベル関数にできる。
          fun <- withMeta (C.typeOf $ Fun ps e) <$> newTemporalId (hint <> "_curry")
          ps' <- traverse (\p -> withMeta p.meta <$> newTemporalId p.id.name) ps
          e' <- alpha (HashMap.fromList $ zip ps $ map C.Var ps') e
          modify \s@DsState {..} -> s {_globalDefs = FunDef fun ps' (C.typeOf fun) e' : _globalDefs}
          let body = C.CallDirect fun $ reverse $ C.Var x : as
          pure ([x], body)
        else do
          fun <- withMeta (C.typeOf $ Fun ps e) <$> newTemporalId (hint <> "_curry")
          let body = C.Call (C.Var fun) $ reverse $ C.Var x : as
          ps' <- traverse (\p -> withMeta p.meta <$> newTemporalId p.id.name) ps
          e' <- alpha (HashMap.fromList $ zip ps $ map C.Var ps') e
          pure ([x], C.Let [LocalDef fun (C.typeOf fun) (Fun ps' e')] body)
    curryFun' (x : xs) as = do
      fun <- curryFun' xs (C.Var x : as)
      let funObj = uncurry Fun fun
      body <- runDef $ do
        fun <- let_ (C.typeOf funObj) funObj
        pure $ Atom fun
      pure ([x], body)

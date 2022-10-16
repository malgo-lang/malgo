{-# LANGUAGE TemplateHaskell #-}

-- | MalgoをKoriel.Coreに変換（脱糖衣）する
module Malgo.Desugar.Pass (desugar) where

import Control.Lens (At (at), makePrisms, preuse, preview, traverseOf, traversed, use, (<>=), (?=), (^.), _2, _Just)
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
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Desugar.DsEnv
import Malgo.Desugar.Match
import Malgo.Desugar.Type
import Malgo.Infer.TcEnv (TcEnv)
import Malgo.Infer.TypeRep as GT
import Malgo.Interface
import Malgo.Prelude
import Malgo.Syntax as G
import Malgo.Syntax.Extension as G
import GHC.Records (HasField)

-- | トップレベル宣言
data Def
  = VarDef (Id C.Type) (C.Exp (Id C.Type))
  | FunDef (Id C.Type) ([Id C.Type], C.Exp (Id C.Type))
  | ExtDef Text C.Type

makePrisms ''Def

-- | MalgoからCoreへの変換
desugar ::
  (MonadReader MalgoEnv m, XModule x ~ BindGroup (Malgo 'Refine), MonadFail m, MonadIO m) =>
  TcEnv ->
  Module x ->
  m (DsState, Program (Id C.Type))
desugar tcEnv (Module modName ds) = do
  malgoEnv <- ask
  runReaderT ?? makeDsEnv modName malgoEnv $ do
    (ds', dsEnv) <- runStateT (dsBindGroup ds) (makeDsState tcEnv)
    let varDefs = mapMaybe (preview _VarDef) ds'
    let funDefs = mapMaybe (preview _FunDef) ds'
    -- let extDefs = map (\dep -> ("koriel_load_" <> coerce dep, [] :-> VoidT)) (List.delete modName depList) <> [("GC_init", [] :-> VoidT)] <> mapMaybe (preview _ExtDef) ds'
    let extDefs = mapMaybe (preview _ExtDef) ds'
    -- case searchMain (HashMap.toList $ view nameEnv dsEnv) of
    --   Just mainCall -> do
    --     mainFuncDef <-
    --       mainFunc depList =<< runDef do
    --         let unitCon = C.Con C.Tuple []
    --         unit <- let_ (SumT [unitCon]) (Pack (SumT [unitCon]) unitCon [])
    --         _ <- bind $ mainCall [unit]
    --         pure (Atom $ C.Unboxed $ C.Int32 0)
    --     pure (dsEnv, Program varDefs (mainFuncDef : funDefs) extDefs)
    --   Nothing -> pure (dsEnv, Program varDefs funDefs extDefs)
    pure (dsEnv, Program varDefs funDefs extDefs)

-- BindGroupの脱糖衣
-- DataDef, Foreign, ScDefの順で処理する
dsBindGroup ::
  (MonadState DsState m, MonadReader env m, MonadFail m, MonadIO m, HasUniqSupply env UniqSupply, HasModulePaths env [FilePath], HasInterfaces env (IORef (HashMap ModuleName Interface)), HasModuleName env ModuleName) =>
  BindGroup (Malgo 'Refine) ->
  m [Def]
dsBindGroup bg = do
  traverse_ dsImport (bg ^. imports)
  dataDefs' <- traverse dsDataDef (bg ^. dataDefs)
  foreigns' <- traverse dsForeign (bg ^. foreigns)
  scDefs' <- dsScDefGroup (bg ^. scDefs)
  pure $ mconcat dataDefs' <> mconcat foreigns' <> scDefs'

dsImport :: (MonadReader env m, MonadState DsState m, MonadIO m, HasModulePaths env [FilePath], HasInterfaces env (IORef (HashMap ModuleName Interface))) => Import (Malgo 'Refine) -> m ()
dsImport (_, modName, _) = do
  interface <- loadInterface modName
  nameEnv <>= interface ^. coreIdentMap

-- ScDefのグループを一つのリストにつぶしてから脱糖衣する
dsScDefGroup ::
  (MonadState DsState f, MonadReader env f, MonadFail f, MonadIO f, HasUniqSupply env UniqSupply, HasModuleName env ModuleName) =>
  [[ScDef (Malgo 'Refine)]] ->
  f [Def]
dsScDefGroup = dsScDefs . mconcat

dsScDefs ::
  (MonadState DsState f, MonadReader env f, MonadFail f, MonadIO f, HasUniqSupply env UniqSupply, HasModuleName env ModuleName) =>
  [ScDef (Malgo 'Refine)] ->
  f [Def]
dsScDefs ds = do
  -- まず、宣言されているScDefの名前をすべて名前環境に登録する
  for_ ds $ \(_, f, _) -> do
    Just (Forall _ fType) <- use (signatureMap . at f)
    f' <- newCoreId f =<< dsType fType
    nameEnv . at f ?= f'
  foldMapM dsScDef ds

dsScDef :: (MonadState DsState f, MonadReader env f, MonadFail f, MonadIO f, HasUniqSupply env UniqSupply, HasModuleName env ModuleName) => ScDef (Malgo 'Refine) -> f [Def]
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
      pure [VarDef name' expr']
    dsFunDef name (G.Fn _ cs)= do
      name' <- lookupName name
      fun <- fnToObj typ cs
      pure [FunDef name' fun]
    dsFunDef name expr = do
      name' <- lookupName name
      typ' <- dsType typ
      fun <- curryFun [] =<< runDef (fmap Atom (cast typ' =<< dsExp expr))
      pure [FunDef name' fun]

fnToObj :: (MonadIO f, HasUniqSupply env UniqSupply, MonadFail f, MonadReader env f, HasModuleName env ModuleName, MonadState DsState f) => GT.Type -> NonEmpty (Clause (Malgo 'Refine)) -> f ([Id C.Type], C.Exp (Id C.Type))
fnToObj typ cs@(Clause _ ps e :| _ ) = do
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
  curryFun ps' body

patToName :: HasField "name" (XId x) Text => Pat x -> Text
patToName (G.VarP _ v) = v.name
patToName (G.ConP _ c _) = T.toLower $ c.name
patToName (G.TupleP _ _) = "tuple"
patToName (G.RecordP _ _) = "record"
patToName (G.ListP _ _) = "list"
patToName (G.UnboxedP _ _) = "unboxed"
patToName (G.BoxedP _ _) = "boxed"

-- TODO: Malgoのforeignでvoid型をあつかえるようにする #13
-- 1. Malgoの型とCの型の相互変換を定義する
-- 2. 相互変換を値に対して行うCoreコードを生成する関数を定義する
-- 3. 2.の関数を使ってdsForeignを書き換える
dsForeign ::
  (MonadState DsState f, MonadIO f, MonadReader env f, HasUniqSupply env UniqSupply, HasModuleName env ModuleName) =>
  Foreign (Malgo 'Refine) ->
  f [Def]
dsForeign (Typed typ (_, primName), name, _) = do
  name' <- newCoreId name =<< dsType typ
  let (paramTypes, retType) = splitTyArr typ
  paramTypes' <- traverse dsType paramTypes
  retType <- dsType retType
  params <- traverse (newTemporalId "p") paramTypes'
  fun <- curryFun params $ C.RawCall primName (paramTypes' :-> retType) (map C.Var params)
  nameEnv . at name ?= name'
  pure [FunDef name' fun, ExtDef primName (paramTypes' :-> retType)]

dsDataDef ::
  (MonadState DsState m, MonadFail m, MonadReader env m, HasUniqSupply env UniqSupply, MonadIO m, HasModuleName env ModuleName) =>
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
    conName' <- newCoreId conName $ buildConType paramTypes' retType'
    ps <- traverse (newTemporalId "p") paramTypes'
    expr <- runDef $ do
      unfoldedType <- unfoldType retType
      packed <- let_ unfoldedType (Pack unfoldedType (C.Con (Data $ idToText conName) paramTypes') $ map C.Var ps)
      pure $ Cast retType' packed
    obj <- case ps of
      [] -> pure ([], expr)
      _ -> curryFun ps expr
    nameEnv . at conName ?= conName'
    pure (FunDef conName' obj)
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
  (MonadState DsState m, MonadIO m, MonadFail m, MonadReader env m, HasUniqSupply env UniqSupply, HasModuleName env ModuleName) =>
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
              pure $ C.Let [LocalDef clsId (Fun ps $ CallDirect name' $ map C.Var ps)] $ Atom $ C.Var clsId
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
  obj <- fnToObj typ cs
  v <- newTemporalId "fun" =<< dsType typ
  pure $ C.Let [C.LocalDef v (uncurry Fun obj)] $ Atom $ C.Var v
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
  pure $ C.Let [C.LocalDef v (C.Record $ HashMap.fromList kvs')] $ Atom $ C.Var v
dsExp (G.Record _ _) = error "unreachable"
dsExp (G.Seq _ ss) = dsStmts ss

dsStmts :: (MonadState DsState m, MonadIO m, MonadFail m, MonadReader env m, HasUniqSupply env UniqSupply, HasModuleName env ModuleName) => NonEmpty (Stmt (Malgo 'Refine)) -> m (C.Exp (Id C.Type))
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
  pure $ Match e' (Bind v' ss' :| [])

-- Desugar Monad

lookupName :: HasCallStack => MonadState DsState m => RnId -> m (Id C.Type)
lookupName name = do
  mname' <- use (nameEnv . at name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint name)

newCoreId :: (MonadReader env f, MonadIO f, HasUniqSupply env UniqSupply) => RnId -> C.Type -> f (Id C.Type)
newCoreId griffId coreType = newIdOnName coreType griffId

-- 関数をカリー化する
curryFun ::
  (HasCallStack, HasModuleName env ModuleName) =>
  (MonadIO m, MonadReader env m, HasUniqSupply env UniqSupply) =>
  -- | パラメータリスト
  [Id C.Type] ->
  -- | カリー化されていない関数値
  C.Exp (Id C.Type) ->
  m ([Id C.Type], C.Exp (Id C.Type))
-- η展開
curryFun [] e = do
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
      curryFun ps body
    _ -> errorDoc $ "Invalid expression:" <+> quotes (pPrint e)
curryFun ps e = curryFun' ps []
  where
    curryFun' [] _ = error "length ps >= 1"
    curryFun' [x] as = do
      fun <- newTemporalId "curry" (C.typeOf $ Fun ps e)
      let body = C.Call (C.Var fun) $ reverse $ C.Var x : as
      pure ([x], C.Let [C.LocalDef fun $ Fun ps e] body)
    curryFun' (x : xs) as = do
      fun <- curryFun' xs (C.Var x : as)
      let funObj = uncurry Fun fun
      body <- runDef $ do
        fun <- let_ (C.typeOf funObj) funObj
        pure $ Atom fun
      pure ([x], body)

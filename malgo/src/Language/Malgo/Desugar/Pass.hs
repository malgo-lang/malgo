{-# LANGUAGE CPP #-}

-- | MalgoをKoriel.Coreに変換（脱糖衣）する
module Language.Malgo.Desugar.Pass (desugar) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Koriel.Core.Syntax as C
import Koriel.Core.Type hiding (Type)
import qualified Koriel.Core.Type as C
import Koriel.Id hiding (newGlobalId, newId)
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Desugar.DsEnv
import Language.Malgo.Desugar.Match
import Language.Malgo.Desugar.Type
import Language.Malgo.Interface
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import Language.Malgo.Syntax as G
import Language.Malgo.Syntax.Extension as G
import Language.Malgo.TypeRep.Static as GT
import Control.Monad (mapAndUnzipM)

-- | MalgoからCoreへの変換
desugar ::
  (MonadReader env m, XModule x ~ BindGroup (Malgo 'Refine), MonadFail m, MonadIO m, HasOpt env, HasUniqSupply env) =>
  HashMap RnId (Scheme GT.Type) ->
  HashMap RnTId (TypeDef GT.Type) ->
  HashMap RnId (Scheme GT.Type) ->
  RnEnv ->
  Module x ->
  m (DsEnv, Program (Id C.Type))
desugar varEnv typeEnv fieldEnv rnEnv (Module modName ds) = do
  (ds', dsEnv) <- runStateT (dsBindGroup ds) (makeDsEnv modName varEnv typeEnv fieldEnv rnEnv)
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
  (MonadState DsEnv m, MonadReader env m, MonadFail m, MonadIO m, HasOpt env, HasUniqSupply env) =>
  BindGroup (Malgo 'Refine) ->
  m [(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]
dsBindGroup bg = do
  traverse_ dsImport (bg ^. imports)
  dataDefs' <- traverse dsDataDef (bg ^. dataDefs)
  foreigns' <- traverse dsForeign (bg ^. foreigns)
  scDefs' <- dsScDefGroup (bg ^. scDefs)
  pure $ mconcat $ mconcat dataDefs' <> foreigns' <> scDefs'

dsImport :: (MonadReader env m, MonadState DsEnv m, MonadIO m, HasOpt env) => Import (Malgo 'Refine) -> m ()
dsImport (_, modName) = do
  interface <- loadInterface modName
  nameEnv <>= interface ^. coreIdentMap

-- 相互再帰するScDefのグループごとに脱糖衣する
dsScDefGroup ::
  (MonadState DsEnv f, MonadReader env f, MonadFail f, HasOpt env, MonadIO f, HasUniqSupply env) =>
  [[ScDef (Malgo 'Refine)]] ->
  f [[(Id C.Type, ([Id C.Type], C.Exp (Id C.Type)))]]
dsScDefGroup = traverse dsScDefs

-- 相互再帰的なグループをdesugar
dsScDefs ::
  (MonadState DsEnv f, MonadReader env f, MonadFail f, HasOpt env, MonadIO f, HasUniqSupply env) =>
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
  (MonadState DsEnv f, MonadReader env f, MonadFail f, HasOpt env, MonadIO f, HasUniqSupply env) =>
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
  (MonadState DsEnv f, MonadIO f, MonadReader env f, HasUniqSupply env) =>
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
  (MonadState DsEnv m, MonadFail m, MonadReader env m, HasUniqSupply env, MonadIO m) =>
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
  (MonadState DsEnv m, MonadIO m, MonadFail m, MonadReader env m, HasUniqSupply env) =>
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
  typ <- dsType =<< GT.typeOf (List.last es)
  -- destruct Clauses
  (pss, es) <-
    mapAndUnzipM
      ( \(Clause _ ps es) ->
          pure (ps, dsStmts es)
      )
      cs
  body <- match ps' (patMatrix pss) es (Error typ)
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
dsExp (G.Record x kvs) = runDef $ do
  kvs' <- traverseOf (traversed . _2) (bind <=< dsExp) kvs
  GT.TyRecord recordType <- pure $ x ^. GT.withType
  kts <- Map.toList <$> traverse dsType recordType
  let con = C.Con C.Tuple $ map snd kts
  let ty = SumT [con]
  let vs' = sortRecordField (map fst kts) kvs'
  tuple <- let_ ty $ Pack ty con vs'
  pure $ Atom tuple
  where
    sortRecordField [] _ = []
    sortRecordField (k : ks) kvs = fromJust (List.lookup k kvs) : sortRecordField ks kvs
dsExp (G.Force _ e) = runDef $ do
  -- lazy valueは0引数関数に変換されるので、その評価は0引数関数の呼び出しになる
  e' <- bind =<< dsExp e
  pure $ Call e' []
dsExp (G.Access x label) = runDef $ do
  GT.TyArr (GT.TyRecord recordType) _ <- pure $ x ^. GT.withType
  kts <- Map.toList <$> traverse dsType recordType
  p <- newLocalId "$p" =<< dsType (GT.TyRecord recordType)
  obj <-
    Fun [p] <$> runDef do
      let con = C.Con C.Tuple $ map snd kts
      tuple <- destruct (Atom (C.Var p)) con
      pure $ Atom $ tuple List.!! fromJust (List.elemIndex label (map fst kts))
  accessType <- dsType (x ^. GT.withType)
  Atom <$> let_ accessType obj
dsExp (G.Parens _ e) = dsExp e

dsStmts :: (MonadState DsEnv m, MonadIO m, MonadFail m, MonadReader env m, HasUniqSupply env) => [Stmt (Malgo 'Refine)] -> m (C.Exp (Id C.Type))
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

-- Desugar Monad

lookupName :: MonadState DsEnv m => RnId -> m (Id C.Type)
lookupName name = do
  mname' <- use (nameEnv . at name)
  case mname' of
    Just name' -> pure name'
    Nothing -> errorDoc $ "Not in scope:" <+> quotes (pPrint name)

newCoreId :: (MonadReader env f, MonadIO f, HasUniqSupply env) => RnId -> C.Type -> f (Id C.Type)
newCoreId griffId coreType = newIdOnName coreType griffId

-- 関数をカリー化する
curryFun ::
  (MonadIO m, MonadReader env m, HasUniqSupply env) =>
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
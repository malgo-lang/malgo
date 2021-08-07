module Malgo.TypeCheck.Pass where

import qualified Data.HashMap.Strict as HashMap (mapKeys)
import Data.List.Extra (anySame)
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Interface (loadInterface, signatureMap, typeAbbrMap, typeDefMap)
import Malgo.Prelude
import Malgo.Rename.RnEnv (RnEnv)
import Malgo.Syntax hiding (Type (..), freevars)
import qualified Malgo.Syntax as S
import Malgo.Syntax.Extension
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep.Static (Rep (..), Scheme (Forall), TypeDef (..), TypeF, typeConstructor, typeParameters, valueConstructors)
import qualified Malgo.TypeRep.Static as Static
import Malgo.TypeRep.UTerm
import Malgo.UTerm
import Malgo.Unify hiding (lookupVar)
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.List as List
import qualified RIO.List.Partial as List
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import Text.Megaparsec (SourcePos)

-------------------------------
-- Lookup the value of TcEnv --
-------------------------------

lookupVar :: (MonadState TcEnv m, HasOpt env, MonadReader env m, MonadIO m, HasLogFunc env) => SourcePos -> RnId -> m (Scheme UType)
lookupVar pos name =
  use (varEnv . at name) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> squotes (pretty name)
    Just scheme -> pure scheme

lookupType :: (MonadState TcEnv m, HasOpt env, MonadReader env m, MonadIO m, HasLogFunc env) => SourcePos -> RnId -> m UType
lookupType pos name =
  preuse (typeEnv . at name . _Just) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> squotes (pretty name)
    Just TypeDef {..} -> pure _typeConstructor

-- fieldsのすべてのフィールドを含むレコード型を検索する
-- マッチするレコード型が複数あった場合はエラー
lookupRecordType :: (MonadState TcEnv m, HasOpt env, MonadReader env m, MonadIO m, HasLogFunc env) => SourcePos -> [WithPrefix RnId] -> m (Scheme UType)
lookupRecordType pos fields = do
  env <- use fieldEnv
  let candidates = map (lookup env) fields
  case List.foldr1 List.intersect candidates of
    [] -> bug $ Unreachable "The existence of fields are proved on Rename pass"
    [(_, scheme)] -> pure scheme
    xs -> errorOn pos $ "Ambiguious record:" <+> sep (punctuate "," $ map (pretty . fst) xs)
  where
    lookup env (WithPrefix (With Nothing k)) = concat $ HashMap.lookup k env
    lookup env (WithPrefix (With (Just p) k)) = filter ((== p) . fst) $ concat $ HashMap.lookup k env

typeCheck :: (MonadFail m, HasOpt env, MonadReader env m, MonadIO m, HasUniqSupply env, HasLogFunc env) => RnEnv -> Module (Malgo 'Rename) -> m (Module (Malgo 'TypeCheck), TcEnv)
typeCheck rnEnv (Module name bg) = do
  tcEnv <- genTcEnv rnEnv
  evalStateT ?? tcEnv $
    runTypeUnifyT $ do
      put tcEnv
      bg' <- tcBindGroup bg
      tcEnv' <- get
      -- FIXME: 自由なUVarに適当なTyVarを束縛する
      -- Right x |> { Right x -> print_Int32 x } みたいなパターンで必要
      -- 今はTyBottomに変換している
      abbrEnv <- use abbrEnv
      zonkedBg <-
        pure bg'
          >>= traverseOf (scDefs . traversed . traversed . _1 . ann) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
          >>= traverseOf (scDefs . traversed . traversed . _3) (walkOn @TypeF @TypeVar (zonk >=> pure . expandAllTypeSynonym abbrEnv))
          >>= traverseOf (foreigns . traversed . _1 . ann) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
      zonkedTcEnv <-
        pure tcEnv'
          >>= traverseOf (varEnv . traversed . traversed) (walkOn @TypeF @TypeVar (zonk >=> pure . expandAllTypeSynonym abbrEnv))
          >>= traverseOf (typeEnv . traversed . traversed) (walkOn @TypeF @TypeVar (zonk >=> pure . expandAllTypeSynonym abbrEnv))
      pure (Module name zonkedBg, zonkedTcEnv)

tcBindGroup ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadIO m,
    HasOpt env,
    MonadReader env m,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  BindGroup (Malgo 'Rename) ->
  m (BindGroup (Malgo 'TypeCheck))
tcBindGroup bindGroup = do
  _imports <- tcImports $ bindGroup ^. imports
  (_typeSynonyms, _dataDefs) <- tcTypeDefinitions (bindGroup ^. typeSynonyms) (bindGroup ^. dataDefs)
  _foreigns <- tcForeigns $ bindGroup ^. foreigns
  _scSigs <- tcScSigs $ bindGroup ^. scSigs
  traverse_ prepareTcScDefs $ bindGroup ^. scDefs
  _scDefs <- tcScDefGroup $ bindGroup ^. scDefs
  pure BindGroup {..}

tcImports ::
  ( MonadState TcEnv m,
    MonadIO m,
    HasOpt env,
    MonadReader env m,
    HasLogFunc env
  ) =>
  [Import (Malgo 'Rename)] ->
  m [Import (Malgo 'TypeCheck)]
tcImports = traverse tcImport
  where
    tcImport (pos, modName, importList) = do
      interface <-
        loadInterface modName >>= \case
          Just x -> pure x
          Nothing -> errorOn pos $ "module" <+> pretty modName <+> "is not found"
      varEnv <>= fmap (fmap Static.fromType) (interface ^. signatureMap)
      typeEnv <>= fmap (fmap Static.fromType) (interface ^. typeDefMap)
      abbrEnv
        <>= HashMap.mapKeys
          (fmap Static.fromType)
          ( fmap
              (over _2 Static.fromType . over (_1 . mapped . idMeta) Static.fromType)
              (interface ^. typeAbbrMap)
          )
      pure (pos, modName, importList)

tcTypeDefinitions ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadReader env m,
    MonadIO m,
    HasOpt env,
    HasUniqSupply env,
    MonadFail m,
    HasLogFunc env
  ) =>
  [TypeSynonym (Malgo 'Rename)] ->
  [DataDef (Malgo 'Rename)] ->
  m ([TypeSynonym (Malgo 'TypeCheck)], [DataDef (Malgo 'TypeCheck)])
tcTypeDefinitions typeSynonyms dataDefs = do
  -- 相互再帰的な型定義がありうるため、型コンストラクタに対応するTyConを先にすべて生成する
  for_ typeSynonyms \(_, name, params, _) -> do
    tyCon <- TyCon <$> newIdOnName (buildTyConKind params) name
    typeEnv . at name .= Just (TypeDef tyCon [] [])
  for_ dataDefs \(_, name, params, _) -> do
    tyCon <- TyCon <$> newIdOnName (buildTyConKind params) name
    typeEnv . at name .= Just (TypeDef tyCon [] [])
  (,) <$> tcTypeSynonyms typeSynonyms
    <*> tcDataDefs dataDefs
  where
    -- TODO: ほんとはpolymorphicな値を返さないといけないと思う
    buildTyConKind [] = TYPE $ Rep BoxedRep
    buildTyConKind (_ : xs) = TyArr (TYPE $ Rep BoxedRep) (buildTyConKind xs)

tcTypeSynonyms ::
  ( MonadBind f,
    MonadState TcEnv f,
    MonadIO f,
    MonadReader env f,
    HasOpt env,
    HasUniqSupply env,
    MonadFail f,
    HasLogFunc env
  ) =>
  [TypeSynonym (Malgo 'Rename)] ->
  f [TypeSynonym (Malgo 'TypeCheck)]
tcTypeSynonyms ds =
  for ds \(pos, name, params, typ) -> do
    TyCon con <- lookupType pos name

    params' <- traverse (\p -> newLocalId (nameToString $ p ^. idName) (TYPE $ Rep BoxedRep)) params
    zipWithM_ (\p p' -> typeEnv . at p .= Just (TypeDef (TyVar p') [] [])) params params'

    typ' <- transType typ
    abbrEnv . at con .= Just (params', typ')
    updateFieldEnv (nameToString $ name ^. idName) (tcType typ) params' typ'

    pure (pos, name, params, tcType typ)

updateFieldEnv :: (MonadState TcEnv f) => RecordTypeName -> S.Type (Malgo 'TypeCheck) -> [Id UType] -> UType -> f ()
updateFieldEnv typeName (S.TyRecord _ kts) params typ = do
  let scheme = Forall params typ
  for_ kts \(label, _) -> do
    modify (appendFieldEnv [(label, (typeName, scheme))])
updateFieldEnv _ _ _ _ = pure ()

tcDataDefs ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadReader env m,
    HasOpt env,
    MonadIO m,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  [DataDef (Malgo 'Rename)] ->
  m [DataDef (Malgo 'TypeCheck)]
tcDataDefs ds = do
  bindedTypeVars <- HashSet.unions . map (freevars . view typeConstructor) . HashMap.elems <$> use typeEnv
  for ds \(pos, name, params, valueCons) -> do
    name' <- lookupType pos name
    params' <- traverse (const $ UVar <$> freshVar) params
    let nameKind = kindOf name'
    let paramKinds = map kindOf params'
    solve [With pos $ buildTyArr paramKinds (TYPE $ Rep BoxedRep) :~ nameKind]
    zipWithM_ (\p p' -> typeEnv . at p .= Just (TypeDef p' [] [])) params params'
    (valueConsNames, valueConsTypes) <-
      unzip <$> forOf (traversed . _2) valueCons \args -> do
        -- 値コンストラクタの型を構築
        -- name' <- lookupType pos name
        -- params' <- traverse (lookupType pos) params
        args' <- traverse transType args
        pure $ buildTyArr args' (buildTyApp name' params')
    -- let valueConsNames = map fst valueCons'
    -- let valueConsTypes = map snd valueCons'
    (as, valueConsTypes') <- generalizeMutRecs pos bindedTypeVars valueConsTypes
    let valueCons' = zip valueConsNames $ map (Forall as) valueConsTypes'
    varEnv <>= HashMap.fromList valueCons'
    typeEnv . at name %= (_Just . typeParameters .~ as) . (_Just . valueConstructors .~ valueCons')
    pure (pos, name, params, map (second (map tcType)) valueCons)

tcForeigns ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadReader env m,
    MonadIO m,
    HasOpt env,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  [Foreign (Malgo 'Rename)] ->
  m [Foreign (Malgo 'TypeCheck)]
tcForeigns ds =
  for ds \((pos, raw), name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar
      typeEnv . at tyVar ?= TypeDef (UVar tv) [] []
    scheme@(Forall _ ty') <- generalize pos mempty =<< transType ty
    varEnv . at name ?= scheme
    pure (With ty' (pos, raw), name, tcType ty)

tcScSigs ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadReader env m,
    MonadIO m,
    HasOpt env,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  [ScSig (Malgo 'Rename)] ->
  m [ScSig (Malgo 'TypeCheck)]
tcScSigs ds =
  for ds \(pos, name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar
      typeEnv . at tyVar ?= TypeDef (UVar tv) [] []
    scheme <- generalize pos mempty =<< transType ty
    varEnv . at name ?= scheme
    pure (pos, name, tcType ty)

prepareTcScDefs :: (MonadState TcEnv m, MonadBind m) => [ScDef (Malgo 'Rename)] -> m ()
prepareTcScDefs = traverse_ \(_, name, _) -> do
  use (varEnv . at name) >>= \case
    Nothing -> do
      ty <- Forall [] . UVar <$> freshVar
      varEnv . at name ?= ty
    Just _ -> pure ()

tcScDefGroup ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasOpt env,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  [[ScDef (Malgo 'Rename)]] ->
  m [[ScDef (Malgo 'TypeCheck)]]
tcScDefGroup = traverse tcScDefs

tcScDefs ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasOpt env,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  [ScDef (Malgo 'Rename)] ->
  m [ScDef (Malgo 'TypeCheck)]
tcScDefs [] = pure []
tcScDefs ds@((pos, _, _) : _) = do
  ds <- for ds \(pos, name, expr) -> do
    (expr', wanted) <- runWriterT (tcExpr expr)
    nameType <- instantiate pos =<< lookupVar pos name
    let exprType = typeOf expr'
    let constraints = With pos (nameType :~ exprType) : wanted
    solve constraints
    exprType <- zonk exprType
    pure (With exprType pos, name, expr')
  (as, types) <- generalizeMutRecs pos mempty $ map (view (_1 . ann)) ds
  -- Validate user-declared type signature and add type schemes to environment
  for_ (zip ds types) \((pos, name, _), inferredSchemeType) -> do
    let inferredScheme = Forall as inferredSchemeType
    declaredScheme <- lookupVar (pos ^. value) name
    case declaredScheme of
      -- No explicit signature
      Forall [] (UVar _) -> varEnv . at name ?= inferredScheme
      _ -> do
        -- 型同士を比較する際には型シノニムを展開する
        abbrEnv <- use abbrEnv
        declaredType <- expandAllTypeSynonym abbrEnv <$> instantiate (pos ^. value) declaredScheme
        inferedType <- expandAllTypeSynonym abbrEnv <$> instantiate (pos ^. value) inferredScheme
        case equiv declaredType inferedType of
          Nothing -> errorOn (pos ^. value) $ vsep ["Signature mismatch", indent 2 ("Declared:" <+> pretty declaredScheme), indent 2 ("Inferred:" <+> pretty inferredScheme)]
          Just subst
            | anySame $ HashMap.elems subst -> errorOn (pos ^. value) $ "Signature too general:" <> line <> nest 2 ("Declared:" <+> pretty declaredScheme) <> line <> nest 2 ("Inferred:" <+> pretty inferredScheme)
            | otherwise -> varEnv . at name ?= declaredScheme
  pure ds

tcExpr ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasOpt env,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  Exp (Malgo 'Rename) ->
  WriterT [With SourcePos Constraint] m (Exp (Malgo 'TypeCheck))
tcExpr (Var pos (WithPrefix (With p v))) = do
  vType <- instantiate pos =<< lookupVar pos v
  pure $ Var (With vType pos) (WithPrefix (With p v))
tcExpr (Unboxed pos u) = do
  let uType = typeOf u
  pure $ Unboxed (With uType pos) u
tcExpr (Apply pos f x) = do
  f' <- tcExpr f
  x' <- tcExpr x
  retType <- UVar <$> freshVar
  tell [With pos $ typeOf f' :~ TyArr (typeOf x') retType]
  pure $ Apply (With retType pos) f' x'
tcExpr (OpApp x@(pos, _) op e1 e2) = do
  e1' <- tcExpr e1
  e2' <- tcExpr e2
  opScheme <- lookupVar pos op
  opType <- instantiate pos opScheme
  retType <- UVar <$> freshVar
  tell [With pos $ opType :~ TyArr (typeOf e1') (TyArr (typeOf e2') retType)]
  pure $ OpApp (With retType x) op e1' e2'
tcExpr (Fn pos (Clause x [] ss :| _)) = do
  ss' <- tcStmts ss
  let ssType = typeOf $ NonEmpty.last ss'
  pure $ Fn (With (TyApp TyLazy ssType) pos) (Clause (With (TyApp TyLazy ssType) x) [] ss' :| [])
tcExpr (Fn pos cs) = do
  traverse tcClause cs >>= \case
    (c' :| cs') -> do
      for_ cs' \c -> tell [With pos $ typeOf c' :~ typeOf c]
      pure $ Fn (With (typeOf c') pos) (c' :| cs')
tcExpr (Tuple pos es) = do
  es' <- traverse tcExpr es
  let esType = buildTyApp (TyTuple $ length es) $ map typeOf es'
  pure $ Tuple (With esType pos) es'
tcExpr (Record pos kvs) = do
  kvs' <- traverse (bitraverse pure tcExpr) kvs
  let kvsType = TyRecord $ Map.fromList $ map (bimap removePrefix typeOf) kvs'
  pure $ Record (With kvsType pos) kvs'
-- レコードリテラルでは、レコード型をフィールド名から検索する必要はない
-- * 存在しないフィールド名はRenameパスでエラーになる
-- * 必ずすべてのフィールド・値ペアがある
-- レコード型を検索するコードは↓
-- recordType <- instantiate pos =<< lookupRecordType pos (map fst kvs)
-- tell [With pos $ recordType :~ kvsType]
-- pure $ Record (With recordType pos) kvs'
tcExpr (Force pos e) = do
  e' <- tcExpr e
  ty <- UVar <$> freshVar
  tell [With pos $ TyApp TyLazy ty :~ typeOf e']
  pure $ Force (With ty pos) e'
tcExpr (RecordAccess pos label) = do
  recordType <- zonk =<< instantiate pos =<< lookupRecordType pos [label]
  retType <- UVar <$> freshVar
  case recordType of
    TyRecord kts -> do
      tell [With pos $ recordType :~ TyRecord (Map.insert (removePrefix label) retType kts)]
      pure $ RecordAccess (With (TyArr recordType retType) pos) label
    _ -> errorOn pos $ pretty recordType <+> "is not record type"
tcExpr (Parens pos e) = do
  e' <- tcExpr e
  pure $ Parens (With (typeOf e') pos) e'

tcClause ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasOpt env,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  Clause (Malgo 'Rename) ->
  WriterT [With SourcePos Constraint] m (Clause (Malgo 'TypeCheck))
tcClause (Clause pos pats ss) = do
  pats' <- tcPatterns pats
  ss' <- tcStmts ss
  let ssType = typeOf $ NonEmpty.last ss'
  let patTypes = map typeOf pats'
  pure $ Clause (With (buildTyArr patTypes ssType) pos) pats' ss'

tcPatterns ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    HasOpt env,
    MonadReader env m,
    HasLogFunc env
  ) =>
  [Pat (Malgo 'Rename)] ->
  WriterT [With SourcePos Constraint] m [Pat (Malgo 'TypeCheck)]
tcPatterns [] = pure []
tcPatterns (VarP x v : ps) = do
  ty <- UVar <$> freshVar
  varEnv . at v ?= Forall [] ty
  ps' <- tcPatterns ps
  pure $ VarP (With ty x) v : ps'
tcPatterns (ConP pos con pats : ps) = do
  conType <- instantiate pos =<< lookupVar pos con
  let (conParams, _) = splitTyArr conType
  -- コンストラクタの型に基づくASTの組み換え
  -- 足りない分を後続のパターン列から補充
  let (morePats, restPs) = List.splitAt (length conParams - length pats) ps
  -- 足りない分（morePats）を補充した残り（restPs）が空でなければ、
  -- 2引数以上の関数での文法エラー
  when (not (null morePats) && not (null restPs)) $
    errorOn pos "Invalid Pattern: You may need to put parentheses"
  pats' <- tcPatterns (pats <> morePats)
  ty <- UVar <$> freshVar
  let patTypes = map typeOf pats'
  tell [With pos $ conType :~ buildTyArr patTypes ty]
  ps' <- tcPatterns restPs
  pure (ConP (With ty pos) con pats' : ps')
tcPatterns (TupleP pos pats : ps) = do
  pats' <- tcPatterns pats
  ps' <- tcPatterns ps
  let patTypes = map typeOf pats'
  pure $ TupleP (With (buildTyApp (TyTuple (length patTypes)) patTypes) pos) pats' : ps'
tcPatterns (RecordP pos kps : ps) = do
  kps' <- traverseOf (traversed . _2) (\x -> List.head <$> tcPatterns [x]) kps
  ps' <- tcPatterns ps

  recordType@(TyRecord recordKts) <- instantiate pos =<< lookupRecordType pos (map fst kps)
  let patternKts = Map.fromList $ map (bimap removePrefix typeOf) kps'
  let patternType = TyRecord $ patternKts <> recordKts

  tell [With pos $ recordType :~ patternType]
  pure $ RecordP (With patternType pos) kps' : ps'
tcPatterns (UnboxedP pos unboxed : ps) = do
  ps' <- tcPatterns ps
  pure $ UnboxedP (With (typeOf unboxed) pos) unboxed : ps'

tcStmts ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadReader env m,
    HasOpt env,
    MonadIO m,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  NonEmpty (Stmt (Malgo 'Rename)) ->
  WriterT [With SourcePos Constraint] m (NonEmpty (Stmt (Malgo 'TypeCheck)))
tcStmts = traverse tcStmt

tcStmt ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadReader env m,
    HasOpt env,
    MonadIO m,
    HasUniqSupply env,
    HasLogFunc env
  ) =>
  Stmt (Malgo 'Rename) ->
  WriterT [With SourcePos Constraint] m (Stmt (Malgo 'TypeCheck))
tcStmt (NoBind pos e) = NoBind pos <$> tcExpr e
tcStmt (Let pos v e) = do
  env <- use varEnv
  envSet <- traverse (zonk . (\(Forall _ t) -> t)) (HashMap.elems env)
  (e', wanted) <- listen $ tcExpr e
  solve wanted
  -- FIXME: value restriction
  vScheme <- generalize pos (mconcat $ map freevars envSet) (typeOf e')
  varEnv . at v ?= vScheme
  pure $ Let pos v e'

-----------------------------------
-- Translate Type representation --
-----------------------------------

transType :: (MonadState TcEnv m, MonadBind m, MonadReader env m, MonadIO m, HasOpt env, HasUniqSupply env, HasLogFunc env) => S.Type (Malgo 'Rename) -> m UType
transType (S.TyApp pos t ts) = do
  rnEnv <- use rnEnv
  let ptr_t = fromJust $ findBuiltinType "Ptr#" rnEnv
  case (t, ts) of
    (S.TyCon _ c, [t]) | c == ptr_t -> do
      t' <- transType t
      rep <- UVar . TypeVar <$> newLocalId "r" TyRep
      solve [With pos $ kindOf t' :~ TYPE rep]
      pure $ TyPtr t'
    _ -> do
      t' <- transType t
      ts' <- traverse transType ts
      solve [With pos $ buildTyArr (map kindOf ts') (TYPE $ Rep BoxedRep) :~ kindOf t']
      buildTyApp <$> transType t <*> traverse transType ts
transType (S.TyVar pos v) = lookupType pos v
transType (S.TyCon pos c) = lookupType pos c
transType (S.TyArr _ t1 t2) = TyArr <$> transType t1 <*> transType t2
transType (S.TyTuple _ ts) = buildTyApp (TyTuple $ length ts) <$> traverse transType ts
transType (S.TyRecord _ kts) = TyRecord . Map.fromList <$> traverseOf (traversed . _2) transType kts
transType (S.TyLazy _ t) = TyApp TyLazy <$> transType t

tcType :: S.Type (Malgo 'Rename) -> S.Type (Malgo 'TypeCheck)
tcType (S.TyApp pos t ts) = S.TyApp pos (tcType t) (map tcType ts)
tcType (S.TyVar pos v) = S.TyVar pos v
tcType (S.TyCon pos c) = S.TyCon pos c
tcType (S.TyArr pos t1 t2) = S.TyArr pos (tcType t1) (tcType t2)
tcType (S.TyTuple pos ts) = S.TyTuple pos $ map tcType ts
tcType (S.TyRecord pos kts) = S.TyRecord pos $ over (mapped . _2) tcType kts
tcType (S.TyLazy pos t) = S.TyLazy pos $ tcType t

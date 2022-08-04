module Malgo.TypeCheck.Pass where

import Control.Lens (At (at), forOf, ix, mapped, over, preuse, traverseOf, traversed, use, view, (%=), (.=), (.~), (<>=), (?=), (^.), _1, _2, _3, _Just)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.List.Extra (anySame)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Interface (loadInterface)
import Malgo.Prelude hiding (Constraint)
import Malgo.Rename.RnEnv (RnEnv)
import Malgo.Syntax hiding (Type (..), freevars)
import qualified Malgo.Syntax as S
import Malgo.Syntax.Extension
import Malgo.TypeCheck.TcEnv
import Malgo.TypeCheck.Unify hiding (lookupVar)
import Malgo.TypeRep

-------------------------------
-- Lookup the value of TcEnv --
-------------------------------

lookupVar :: (MonadState TcEnv m, MonadReader env m, MonadIO m, HasSrcName env FilePath) => Range -> RnId -> m (Scheme Type)
lookupVar pos name =
  use (signatureMap . at name) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just scheme -> pure scheme

lookupType :: (MonadState TcEnv m, MonadReader env m, MonadIO m, HasSrcName env FilePath) => Range -> RnId -> m Type
lookupType pos name =
  preuse (typeDefMap . ix name) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just TypeDef {..} -> pure _typeConstructor

-- | fieldsのすべてのフィールドを含むレコード型を検索する
-- | マッチするレコード型が複数あった場合はエラー
exactMatchRecordType :: (MonadState TcEnv m, MonadReader env m, MonadIO m, HasSrcName env FilePath) => Range
  -> [Text] -- ^ full field list of the wanted record type
  -> m (Scheme Type)
exactMatchRecordType pos fields = do
  env <- use fieldBelongMap
  let candidates = concat $ HashMap.lookup fields env 
  case candidates of
    [] -> errorOn pos $ "The existence of fields are proved on Rename pass" $$ "fields:" <+> pPrint fields
    [(_, scheme)] -> pure scheme
    xs -> errorOn pos $ "Ambiguious record:" <+> sep (punctuate "," $ map pPrint xs)

typeCheck :: (MonadFail m, MonadIO m) => RnEnv -> Module (Malgo 'Rename) -> m (Module (Malgo 'TypeCheck), TcEnv)
typeCheck rnEnv (Module name bg) = runReaderT ?? rnEnv $ do
  tcEnv <- genTcEnv rnEnv
  evalStateT ?? tcEnv $
    runTypeUnifyT $ do
      put tcEnv
      bg' <- tcBindGroup bg
      tcEnv' <- get
      -- FIXME: 自由なTyMetaに適当なTyVarを束縛する
      -- Right x |> { Right x -> print_Int32 x } みたいなパターンで必要
      -- 今はAnyTに変換している
      abbrEnv <- use typeSynonymMap
      zonkedBg <-
        pure bg'
          >>= traverseOf (scDefs . traversed . traversed . _1 . ann) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
          >>= traverseOf (scDefs . traversed . traversed . _3 . types) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
          >>= traverseOf (foreigns . traversed . _1 . ann) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
      zonkedTcEnv <-
        pure tcEnv'
          >>= traverseOf (signatureMap . traversed . traversed . types) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
          >>= traverseOf (typeDefMap . traversed . traversed . types) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
      pure (Module name zonkedBg, zonkedTcEnv)

tcBindGroup ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasUniqSupply env UniqSupply,
    HasModulePaths env [FilePath],
    HasSrcName env FilePath
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
    MonadReader env m,
    HasModulePaths env [FilePath],
    HasSrcName env FilePath
  ) =>
  [Import (Malgo 'Rename)] ->
  m [Import (Malgo 'TypeCheck)]
tcImports = traverse tcImport
  where
    tcImport (pos, modName, importList) = do
      interface <-
        loadInterface modName >>= \case
          Just x -> pure x
          Nothing -> errorOn pos $ "module" <+> pPrint modName <+> "is not found"
      signatureMap <>= (interface ^. signatureMap)
      typeDefMap <>= (interface ^. typeDefMap)
      typeSynonymMap <>= (interface ^. typeSynonymMap)
      pure (pos, modName, importList)

tcTypeDefinitions ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadReader env m,
    MonadIO m,
    HasUniqSupply env UniqSupply,
    MonadFail m,
    HasSrcName env FilePath
  ) =>
  [TypeSynonym (Malgo 'Rename)] ->
  [DataDef (Malgo 'Rename)] ->
  m ([TypeSynonym (Malgo 'TypeCheck)], [DataDef (Malgo 'TypeCheck)])
tcTypeDefinitions typeSynonyms dataDefs = do
  -- 相互再帰的な型定義がありうるため、型コンストラクタに対応するTyConを先にすべて生成する
  for_ typeSynonyms \(_, name, params, _) -> do
    tyCon <- TyCon <$> newIdOnName (buildTyConKind params) name
    typeDefMap . at name .= Just (TypeDef tyCon [] [])
  for_ dataDefs \(_, name, params, _) -> do
    tyCon <- TyCon <$> newIdOnName (buildTyConKind params) name
    typeDefMap . at name .= Just (TypeDef tyCon [] [])
  typeSynonyms' <- tcTypeSynonyms typeSynonyms
  dataDefs' <- tcDataDefs dataDefs
  pure (typeSynonyms', dataDefs')
  where
    buildTyConKind [] = TYPE
    buildTyConKind (_ : xs) = TyArr TYPE (buildTyConKind xs)

tcTypeSynonyms ::
  ( MonadBind f,
    MonadState TcEnv f,
    MonadIO f,
    MonadReader env f,
    HasUniqSupply env UniqSupply,
    MonadFail f,
    HasSrcName env FilePath
  ) =>
  [TypeSynonym (Malgo 'Rename)] ->
  f [TypeSynonym (Malgo 'TypeCheck)]
tcTypeSynonyms ds =
  for ds \(pos, name, params, typ) -> do
    TyCon con <- lookupType pos name
    params' <- traverse (\p -> newInternalId (idToText p) TYPE) params
    zipWithM_ (\p p' -> typeDefMap . at p .= Just (TypeDef (TyVar p') [] [])) params params'
    typ' <- transType typ
    typeSynonymMap . at con .= Just (params', typ')
    updateFieldEnv (name ^. idName) (tcType typ) params' typ'

    pure (pos, name, params, tcType typ)

updateFieldEnv :: (MonadState TcEnv f) => RecordTypeName -> S.Type (Malgo 'TypeCheck) -> [Id Type] -> Type -> f ()
updateFieldEnv typeName (S.TyRecord _ kts) params typ = do
  let scheme = Forall params typ
  let labels = map (view _1) kts
  modify (appendFieldBelongMap [(labels, (typeName, scheme))])
updateFieldEnv _ _ _ _ = pass

tcDataDefs ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadReader env m,
    MonadIO m,
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
  ) =>
  [DataDef (Malgo 'Rename)] ->
  m [DataDef (Malgo 'TypeCheck)]
tcDataDefs ds = do
  for ds \(pos, name, params, valueCons) -> do
    -- 1. 宣言から、各コンストラクタの型シグネチャを生成する
    name' <- lookupType pos name
    params' <- traverse (\p -> newInternalId (idToText $ p ^. value) TYPE) params
    zipWithM_ (\p p' -> typeDefMap . at (p ^. value) .= Just (TypeDef (TyVar p') [] [])) params params'
    (_, valueConsNames, valueConsTypes) <-
      unzip3 <$> forOf (traversed . _3) valueCons \args -> do
        -- 値コンストラクタの型を構築
        args' <- traverse transType args
        pure $ buildTyArr args' (TyConApp name' $ map TyVar params')
    let valueCons' = zip valueConsNames $ map (Forall params') valueConsTypes
    signatureMap <>= HashMap.fromList valueCons'
    -- 2. 環境に登録する
    typeDefMap . at name %= (_Just . typeParameters .~ params') . (_Just . valueConstructors .~ valueCons')
    pure (pos, name, params, map (second (map tcType)) valueCons)

tcForeigns ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadReader env m,
    MonadIO m,
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
  ) =>
  [Foreign (Malgo 'Rename)] ->
  m [Foreign (Malgo 'TypeCheck)]
tcForeigns ds =
  for ds \((pos, raw), name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar $ Just $ tyVar ^. idName
      typeDefMap . at tyVar ?= TypeDef (TyMeta tv) [] []
    ty' <- transType ty
    scheme@(Forall _ ty') <- generalize pos mempty ty'
    signatureMap . at name ?= scheme
    pure (Annotated ty' (pos, raw), name, tcType ty)

tcScSigs ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadReader env m,
    MonadIO m,
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
  ) =>
  [ScSig (Malgo 'Rename)] ->
  m [ScSig (Malgo 'TypeCheck)]
tcScSigs ds =
  for ds \(pos, name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar $ Just $ tyVar ^. idName
      typeDefMap . at tyVar ?= TypeDef (TyMeta tv) [] []
    scheme <- generalize pos mempty =<< transType ty
    signatureMap . at name ?= scheme
    pure (pos, name, tcType ty)

prepareTcScDefs :: (MonadState TcEnv m, MonadBind m) => [ScDef (Malgo 'Rename)] -> m ()
prepareTcScDefs = traverse_ \(_, name, _) ->
  whenNothingM_
    (use (signatureMap . at name))
    ( do
        ty <- Forall [] . TyMeta <$> freshVar Nothing
        signatureMap . at name ?= ty
    )

tcScDefGroup ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
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
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
  ) =>
  [ScDef (Malgo 'Rename)] ->
  m [ScDef (Malgo 'TypeCheck)]
tcScDefs [] = pure []
tcScDefs ds@((pos, _, _) : _) = do
  ds <- traverse tcScDef ds
  -- generalize mutually recursive functions
  (as, types) <- generalizeMutRecs pos mempty $ map (view (_1 . ann)) ds
  validateSignatures ds (as, types)
  pure ds

-- | Infer types of a function (or variable)
--
-- `tcScDef` does *not* to generalize that types.
--
-- We need to generalize them by `generalizeMutRecs` and validate them signatures by `validateSignatures`
tcScDef ::
  (MonadReader env m, MonadIO m, MonadFail m, MonadState TcEnv m, MonadBind m, HasUniqSupply env UniqSupply, HasSrcName env FilePath) =>
  ScDef (Malgo 'Rename) ->
  m (ScDef (Malgo 'TypeCheck))
tcScDef (pos, name, expr) = do
  (expr', wanted) <- runWriterT (tcExpr expr)
  nameType <- instantiate pos =<< lookupVar pos name
  let exprType = typeOf expr'
  let constraints = Annotated pos (nameType :~ exprType) : wanted
  solve constraints
  exprType <- zonk exprType
  pure (Annotated exprType pos, name, expr')

-- | Validate user-declared type signature and add type schemes to environment

-- $howcheck

validateSignatures ::
  (MonadReader env m, HasSrcName env FilePath, MonadState TcEnv m, MonadIO m) =>
  -- | definitions of mutualy recursive functions
  [ScDef (Malgo 'TypeCheck)] ->
  -- | signatures of mutualy recursive functions
  ([Id Type], [Type]) ->
  m ()
validateSignatures ds (as, types) = zipWithM_ checkSingle ds types
  where
    -- check single case
    checkSingle (pos, name, _) inferredSchemeType = do
      declaredScheme <- lookupVar (pos ^. value) name
      let inferredScheme = Forall as inferredSchemeType
      case declaredScheme of
        -- No explicit signature
        Forall [] (TyMeta _) -> signatureMap . at name ?= inferredScheme
        _ -> do
          -- 型同士を比較する際には型シノニムを展開する
          -- When we need to bind two or more variables to a single variable,
          -- the declared signature is more general than the inferred one.
          --   Example:
          --     declared: forall a b. a -> b -> a
          --     inferred: forall   x. x -> x -> x
          --       evidence = [a -> x, b -> x] Error! Declared is too general
          --
          --    declared: forall a b. a -> b -> a
          --    inferred: forall x y. x -> y -> x
          --     evidence = [a -> x, b -> y] OK! Declared is well matched with inferred
          abbrEnv <- use typeSynonymMap
          let Forall _ declaredType = fmap (expandAllTypeSynonym abbrEnv) declaredScheme
          let Forall _ inferredType = fmap (expandAllTypeSynonym abbrEnv) inferredScheme
          case evidenceOfEquiv declaredType inferredType of
            Just evidence
              | anySame $ Map.elems evidence -> errorOn (pos ^. value) $ "Signature too general:" $$ nest 2 ("Declared:" <+> pPrint declaredScheme) $$ nest 2 ("TypeCheckred:" <+> pPrint inferredScheme)
              | otherwise -> signatureMap . at name ?= declaredScheme
            Nothing ->
              errorOn (pos ^. value) $
                "Signature mismatch:"
                  $$ nest 2 ("Declared:" <+> pPrint declaredScheme)
                  $$ nest 2 ("TypeCheckred:" <+> pPrint inferredScheme)

-- | Which combination of variables should be unification to consider two types as equal?
-- Use in `tcScDefs`.
evidenceOfEquiv ::
  -- | declared type (∀ was stripped)
  Type ->
  -- | inferred type (∀ was stripped)
  Type ->
  -- | evidence of equivalence (or no evidence)
  Maybe (Map Type Type)
evidenceOfEquiv (TyMeta v1) (TyMeta v2)
  | v1 == v2 = Just mempty
  | otherwise = Just $ one (TyMeta v1, TyMeta v2)
evidenceOfEquiv (TyVar v1) (TyVar v2)
  | v1 == v2 = Just mempty
  | otherwise = Just $ one (TyVar v1, TyVar v2)
evidenceOfEquiv (TyApp t11 t12) (TyApp t21 t22) = (<>) <$> evidenceOfEquiv t11 t21 <*> evidenceOfEquiv t12 t22
evidenceOfEquiv (TyCon c1) (TyCon c2) | c1 == c2 = Just mempty
evidenceOfEquiv (TyPrim p1) (TyPrim p2) | p1 == p2 = Just mempty
evidenceOfEquiv (TyArr l1 r1) (TyArr l2 r2) = (<>) <$> evidenceOfEquiv l1 l2 <*> evidenceOfEquiv r1 r2
evidenceOfEquiv (TyTuple n1) (TyTuple n2) | n1 == n2 = Just mempty
evidenceOfEquiv (TyRecord kts1) (TyRecord kts2) | HashMap.keys kts1 == HashMap.keys kts2 = mconcat <$> zipWithM evidenceOfEquiv (HashMap.elems kts1) (HashMap.elems kts2)
evidenceOfEquiv (TyPtr t1) (TyPtr t2) = evidenceOfEquiv t1 t2
evidenceOfEquiv TYPE TYPE = Just mempty
evidenceOfEquiv _ _ = Nothing

tcExpr ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
  ) =>
  Exp (Malgo 'Rename) ->
  WriterT [Annotated Range Constraint] m (Exp (Malgo 'TypeCheck))
tcExpr (Var pos v) = do
  vType <- instantiate pos =<< lookupVar pos v
  pure $ Var (Annotated vType pos) v
tcExpr (Unboxed pos u) = do
  let uType = typeOf u
  pure $ Unboxed (Annotated uType pos) u
tcExpr (Apply pos f x) = do
  f' <- tcExpr f
  x' <- tcExpr x
  retType <- TyMeta <$> freshVar Nothing
  tell [Annotated pos $ typeOf f' :~ TyArr (typeOf x') retType]
  pure $ Apply (Annotated retType pos) f' x'
tcExpr (OpApp x@(pos, _) op e1 e2) = do
  e1' <- tcExpr e1
  e2' <- tcExpr e2
  opScheme <- lookupVar pos op
  opType <- instantiate pos opScheme
  retType <- TyMeta <$> freshVar Nothing
  tell [Annotated pos $ opType :~ TyArr (typeOf e1') (TyArr (typeOf e2') retType)]
  pure $ OpApp (Annotated retType x) op e1' e2'
tcExpr (Fn pos (Clause x [] e :| _)) = do
  e' <- tcExpr e
  hole <- newInternalId "$_" ()
  signatureMap . at hole ?= Forall [] (TyTuple 0)
  pure $ Fn (Annotated (TyArr (TyTuple 0) (typeOf e')) pos) (Clause (Annotated (TyArr (TyTuple 0) (typeOf e')) x) [VarP (Annotated (TyTuple 0) pos) hole] e' :| [])
tcExpr (Fn pos cs) = do
  (c' :| cs') <- traverse tcClause cs
  -- パターンの数がすべての節で同じかを検査
  -- tcPatternsでパターンの組み換えを行うので、このタイミングで検査する
  -- TODO: エラーメッセージをもっとわかりやすく
  let patNums :: Int = countPatNums c'
  for_ cs' \c -> do
    when (countPatNums c /= patNums) $
      errorOn pos $
        sep
          [ nest 4 $ pPrint (patOf c) <+> "has" <+> pPrint (countPatNums c) <+> "patterns,",
            "but" <+> pPrint (patOf c') <+> "has" <+> pPrint patNums
          ]
    tell [Annotated pos $ typeOf c' :~ typeOf c]
  pure $ Fn (Annotated (typeOf c') pos) (c' :| cs')
  where
    countPatNums (Clause _ ps _) = length ps
    patOf (Clause _ ps _) = ps
tcExpr (Tuple pos es) = do
  es' <- traverse tcExpr es
  let esType = TyConApp (TyTuple $ length es) $ map typeOf es'
  pure $ Tuple (Annotated esType pos) es'
tcExpr (Record pos kvs) = do
  kvs' <- traverse (bitraverse pure tcExpr) kvs
  -- レコードリテラルでは、レコード型をフィールド名から検索する必要はない
  let kvsType = TyRecord $ HashMap.fromList $ map (bimap identity typeOf) kvs'
  pure $ Record (Annotated kvsType pos) kvs'
tcExpr (RecordAccess pos label) = undefined
  -- recordType <- instantiate pos =<< lookupRecordType pos label
  -- retType <- TyMeta <$> freshVar Nothing
  -- case recordType of
  --   TyRecord kts -> do
  --     tell [Annotated pos $ recordType :~ TyRecord (HashMap.insert label retType kts)]
  --     pure $ RecordAccess (Annotated (TyArr recordType retType) pos) label
  --   _ -> errorOn pos $ pPrint recordType <+> "is not record type"
tcExpr (Ann pos e t) = do
  e' <- tcExpr e
  typeRep <- transType t
  tell [Annotated pos $ typeOf e' :~ typeRep]
  pure e'
tcExpr (Seq pos ss) = do
  ss' <- tcStmts ss
  pure $ Seq (Annotated (typeOf $ last ss') pos) ss'
tcExpr (Parens pos e) = do
  e' <- tcExpr e
  pure $ Parens (Annotated (typeOf e') pos) e'

tcClause ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
  ) =>
  Clause (Malgo 'Rename) ->
  WriterT [Annotated Range Constraint] m (Clause (Malgo 'TypeCheck))
tcClause (Clause pos pats e) = do
  pats' <- tcPatterns pats
  e' <- tcExpr e
  let patTypes = map typeOf pats'
  pure $ Clause (Annotated (buildTyArr patTypes (typeOf e')) pos) pats' e'

tcPatterns ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader env m,
    HasSrcName env FilePath
  ) =>
  [Pat (Malgo 'Rename)] ->
  WriterT [Annotated Range Constraint] m [Pat (Malgo 'TypeCheck)]
tcPatterns [] = pure []
tcPatterns (VarP x v : ps) = do
  ty <- TyMeta <$> freshVar Nothing
  signatureMap . at v ?= Forall [] ty
  ps' <- tcPatterns ps
  pure $ VarP (Annotated ty x) v : ps'
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
  ty <- TyMeta <$> freshVar Nothing
  let patTypes = map typeOf pats'
  tell [Annotated pos $ conType :~ buildTyArr patTypes ty]
  ps' <- tcPatterns restPs
  pure (ConP (Annotated ty pos) con pats' : ps')
tcPatterns (TupleP pos pats : ps) = do
  pats' <- tcPatterns pats
  ps' <- tcPatterns ps
  let patTypes = map typeOf pats'
  pure $ TupleP (Annotated (TyConApp (TyTuple (length patTypes)) patTypes) pos) pats' : ps'
tcPatterns (RecordP pos kps : ps) = do
  kps' <- traverseOf (traversed . _2) (\x -> List.head <$> tcPatterns [x]) kps
  ps' <- tcPatterns ps
  recordType@(TyRecord recordKts) <- instantiate pos =<< exactMatchRecordType pos (map fst kps)
  let patternKts = HashMap.fromList $ map (bimap identity typeOf) kps'
  let patternType = TyRecord $ patternKts <> recordKts

  tell [Annotated pos $ recordType :~ patternType]
  pure $ RecordP (Annotated patternType pos) kps' : ps'
tcPatterns (UnboxedP pos unboxed : ps) = do
  ps' <- tcPatterns ps
  pure $ UnboxedP (Annotated (typeOf unboxed) pos) unboxed : ps'

tcStmts ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadReader env m,
    MonadIO m,
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
  ) =>
  NonEmpty (Stmt (Malgo 'Rename)) ->
  WriterT [Annotated Range Constraint] m (NonEmpty (Stmt (Malgo 'TypeCheck)))
tcStmts = traverse tcStmt

tcStmt ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadReader env m,
    MonadIO m,
    HasUniqSupply env UniqSupply,
    HasSrcName env FilePath
  ) =>
  Stmt (Malgo 'Rename) ->
  WriterT [Annotated Range Constraint] m (Stmt (Malgo 'TypeCheck))
tcStmt (NoBind pos e) = NoBind pos <$> tcExpr e
tcStmt (Let pos v e) = do
  e' <- tcExpr e
  signatureMap . at v ?= Forall [] (typeOf e')
  pure $ Let pos v e'

-----------------------------------
-- Translate Type representation --
-----------------------------------

transType :: (MonadState TcEnv m, MonadBind m, MonadReader env m, MonadIO m, HasUniqSupply env UniqSupply, HasSrcName env FilePath) => S.Type (Malgo 'Rename) -> m Type
transType (S.TyApp pos t ts) = do
  tcEnv <- get
  let ptr_t = fromJust $ findBuiltinType "Ptr#" tcEnv
  case (t, ts) of
    (S.TyCon _ c, [t]) | c == ptr_t -> do
      t' <- transType t
      solve [Annotated pos $ kindOf t' :~ TYPE]
      pure $ TyPtr t'
    _ -> do
      t' <- transType t
      ts' <- traverse transType ts
      solve [Annotated pos $ buildTyArr (map kindOf ts') TYPE :~ kindOf t']
      TyConApp <$> transType t <*> traverse transType ts
transType (S.TyVar pos v) = lookupType pos v
transType (S.TyCon pos c) = lookupType pos c
transType (S.TyArr _ t1 t2) = TyArr <$> transType t1 <*> transType t2
transType (S.TyTuple _ ts) = TyConApp (TyTuple $ length ts) <$> traverse transType ts
transType (S.TyRecord _ kts) = TyRecord . HashMap.fromList <$> traverseOf (traversed . _2) transType kts

tcType :: S.Type (Malgo 'Rename) -> S.Type (Malgo 'TypeCheck)
tcType (S.TyApp pos t ts) = S.TyApp pos (tcType t) (map tcType ts)
tcType (S.TyVar pos v) = S.TyVar pos v
tcType (S.TyCon pos c) = S.TyCon pos c
tcType (S.TyArr pos t1 t2) = S.TyArr pos (tcType t1) (tcType t2)
tcType (S.TyTuple pos ts) = S.TyTuple pos $ map tcType ts
tcType (S.TyRecord pos kts) = S.TyRecord pos $ over (mapped . _2) tcType kts

module Malgo.Infer.Pass (infer) where

import Control.Lens (At (at), forOf, ix, mapped, over, preuse, to, traverseOf, traversed, use, view, (%=), (.=), (.~), (<>=), (?=), (^.), _1, _2, _3, _Just)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.Extra (anySame)
import Data.Map qualified as Map
import Data.Traversable (for)
import Error.Diagnose (Marker (This, Where), Report (..), addFile, addReport, def, defaultStyle, printDiagnostic)
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Language.LSP.Types.Lens (HasRange (range))
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Infer.Unify hiding (lookupVar)
import Malgo.Interface (loadInterface)
import Malgo.Prelude hiding (Constraint)
import Malgo.Rename.RnEnv (RnEnv (moduleName, uniqSupply))
import Malgo.Syntax hiding (Type (..))
import Malgo.Syntax qualified as S
import Malgo.Syntax.Extension
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec (sourceName)

-------------------------------
-- Lookup the value of TcEnv --
-------------------------------

lookupVar :: (MonadState TcEnv m, MonadIO m) => Range -> RnId -> m (Scheme Type)
lookupVar pos name =
  use (signatureMap . at name) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just scheme -> pure scheme

lookupType :: (MonadState TcEnv m, MonadIO m) => Range -> RnId -> m Type
lookupType pos name =
  preuse (typeDefMap . ix name) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just TypeDef {..} -> pure _typeConstructor

infer :: (MonadFail m, MonadIO m) => RnEnv -> Module (Malgo 'Rename) -> m (Module (Malgo 'Infer), TcEnv)
infer rnEnv (Module name bg) = runReaderT ?? rnEnv $ do
  tcEnv <- genTcEnv rnEnv
  evalStateT ?? tcEnv $
    runTypeUnifyT $ do
      put tcEnv
      bg' <- tcBindGroup bg
      abbrEnv <- use typeSynonymMap
      zonkedBg <-
        traverseOf (scDefs . traversed . traversed . _1 . types) (zonk >=> pure . expandAllTypeSynonym abbrEnv) bg'
          >>= traverseOf (scDefs . traversed . traversed . _3 . types) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
          >>= traverseOf (foreigns . traversed . _1 . types) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
      zonkedTcEnv <-
        get
          >>= traverseOf (signatureMap . traversed . traversed . types) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
          >>= traverseOf (typeDefMap . traversed . traversed . types) (zonk >=> pure . expandAllTypeSynonym abbrEnv)
      pure (Module name zonkedBg, zonkedTcEnv)

tcBindGroup ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadIO m,
    MonadReader RnEnv m
  ) =>
  BindGroup (Malgo 'Rename) ->
  m (BindGroup (Malgo 'Infer))
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
    MonadReader RnEnv m
  ) =>
  [Import (Malgo 'Rename)] ->
  m [Import (Malgo 'Infer)]
tcImports = traverse tcImport
  where
    tcImport (pos, modName, importList) = do
      interface <- loadInterface modName
      signatureMap <>= (interface ^. signatureMap)
      typeDefMap <>= (interface ^. typeDefMap)
      typeSynonymMap <>= (interface ^. typeSynonymMap)
      kindCtx <>= (interface ^. kindCtx)
      pure (pos, modName, importList)

tcTypeDefinitions ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadReader RnEnv m,
    MonadIO m,
    MonadFail m
  ) =>
  [TypeSynonym (Malgo 'Rename)] ->
  [DataDef (Malgo 'Rename)] ->
  m ([TypeSynonym (Malgo 'Infer)], [DataDef (Malgo 'Infer)])
tcTypeDefinitions typeSynonyms dataDefs = do
  -- 相互再帰的な型定義がありうるため、型コンストラクタに対応するTyConを先にすべて生成する
  for_ typeSynonyms \(_, name, params, _) -> do
    let tyCon = name
    kindCtx %= insertKind tyCon (buildTyConKind params)
    typeDefMap . at name .= Just (TypeDef (TyCon tyCon) [] [])
  for_ dataDefs \(_, name, params, _) -> do
    let tyCon = name
    kindCtx %= insertKind tyCon (buildTyConKind params)
    typeDefMap . at name .= Just (TypeDef (TyCon tyCon) [] [])
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
    MonadReader RnEnv f,
    MonadFail f
  ) =>
  [TypeSynonym (Malgo 'Rename)] ->
  f [TypeSynonym (Malgo 'Infer)]
tcTypeSynonyms ds =
  for ds \(pos, name, params, typ) -> do
    TyCon con <- lookupType pos name
    params' <- for params \p -> newInternalId (idToText p) ()
    zipWithM_ (\p p' -> typeDefMap . at p .= Just (TypeDef (TyVar p') [] [])) params params'
    typ' <- transType typ
    typeSynonymMap . at con .= Just (params', typ')

    pure (pos, name, params, tcType typ)

tcDataDefs ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadReader RnEnv m,
    MonadIO m
  ) =>
  [DataDef (Malgo 'Rename)] ->
  m [DataDef (Malgo 'Infer)]
tcDataDefs ds = do
  for ds \(pos, name, params, valueCons) -> do
    -- 1. 宣言から、各コンストラクタの型シグネチャを生成する
    name' <- lookupType pos name
    params' <- for params \(_, p) -> newInternalId (idToText p) ()
    zipWithM_ (\(_, p) p' -> typeDefMap . at p .= Just (TypeDef (TyVar p') [] [])) params params'
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
    MonadReader RnEnv m,
    MonadIO m
  ) =>
  [Foreign (Malgo 'Rename)] ->
  m [Foreign (Malgo 'Infer)]
tcForeigns ds =
  for ds \((pos, raw), name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar $ Just $ tyVar.name
      typeDefMap . at tyVar ?= TypeDef (TyMeta tv) [] []
    ty' <- transType ty
    scheme@(Forall _ ty') <- generalize pos ty'
    signatureMap . at name ?= scheme
    pure (Typed ty' (pos, raw), name, tcType ty)

tcScSigs ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadReader RnEnv m,
    MonadIO m
  ) =>
  [ScSig (Malgo 'Rename)] ->
  m [ScSig (Malgo 'Infer)]
tcScSigs ds =
  for ds \(pos, name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar $ Just $ tyVar.name
      typeDefMap . at tyVar ?= TypeDef (TyMeta tv) [] []
    scheme <- generalize pos =<< transType ty
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
    MonadReader RnEnv m
  ) =>
  [[ScDef (Malgo 'Rename)]] ->
  m [[ScDef (Malgo 'Infer)]]
tcScDefGroup = traverse tcScDefs

tcScDefs ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader RnEnv m
  ) =>
  [ScDef (Malgo 'Rename)] ->
  m [ScDef (Malgo 'Infer)]
tcScDefs [] = pure []
tcScDefs ds@((pos, _, _) : _) = do
  ds <- traverse tcScDef ds
  -- generalize mutually recursive functions
  (as, types) <- generalizeMutRecs pos $ map (view (_1 . to typeOf)) ds
  validateSignatures ds (as, types)
  pure ds

-- | Infer types of a function (or variable)
--
-- `tcScDef` does *not* to generalize inferred types.
--
-- We need to generalize them by `generalizeMutRecs` and validate them signatures by `validateSignatures`
tcScDef ::
  (MonadReader RnEnv m, MonadIO m, MonadFail m, MonadState TcEnv m, MonadBind m) =>
  ScDef (Malgo 'Rename) ->
  m (ScDef (Malgo 'Infer))
tcScDef (pos, name, expr) = do
  (expr', wanted) <- runWriterT (tcExpr expr)
  nameType <- instantiate pos =<< lookupVar pos name
  let exprType = typeOf expr'
  let constraints = (pos, nameType :~ exprType) : wanted
  solve constraints
  pure (Typed exprType pos, name, expr')

-- | Validate user-declared type signature and add type schemes to environment
validateSignatures ::
  (MonadState TcEnv m, MonadIO m) =>
  -- | definitions of mutualy recursive functions
  [ScDef (Malgo 'Infer)] ->
  -- | signatures of mutualy recursive functions
  ([TypeVar], [Type]) ->
  m ()
validateSignatures ds (as, types) = zipWithM_ checkSingle ds types
  where
    -- check single case
    checkSingle (pos, name, _) inferredSchemeType = do
      declaredScheme <- lookupVar (pos.value) name
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
              | anySame $ Map.elems evidence -> errorOn (pos.value) $ "Signature too general:" $$ nest 2 ("Declared:" <+> pPrint declaredScheme) $$ nest 2 ("Inferred:" <+> pPrint inferredScheme)
              | otherwise -> signatureMap . at name ?= declaredScheme
            Nothing ->
              errorOn (pos.value) $
                "Signature mismatch:"
                  $$ nest 2 ("Declared:" <+> pPrint declaredScheme)
                  $$ nest 2 ("Inferred:" <+> pPrint inferredScheme)

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
evidenceOfEquiv TyPtr TyPtr = Just mempty
evidenceOfEquiv TYPE TYPE = Just mempty
evidenceOfEquiv _ _ = Nothing

tcExpr ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader RnEnv m
  ) =>
  Expr (Malgo 'Rename) ->
  WriterT [(Range, Constraint)] m (Expr (Malgo 'Infer))
tcExpr (Var pos v) = do
  vType <- instantiate pos =<< lookupVar pos v
  pure $ Var (Typed vType pos) v
tcExpr (Unboxed pos u) = do
  let uType = typeOf u
  pure $ Unboxed (Typed uType pos) u
tcExpr (Apply pos f x) = do
  f' <- tcExpr f
  x' <- tcExpr x
  retType <- TyMeta <$> freshVar Nothing
  tell [(pos, typeOf f' :~ TyArr (typeOf x') retType)]
  pure $ Apply (Typed retType pos) f' x'
tcExpr (OpApp x@(pos, _) op e1 e2) = do
  e1' <- tcExpr e1
  e2' <- tcExpr e2
  opScheme <- lookupVar pos op
  opType <- instantiate pos opScheme
  retType <- TyMeta <$> freshVar Nothing
  tell [(pos, opType :~ TyArr (typeOf e1') (TyArr (typeOf e2') retType))]
  pure $ OpApp (Typed retType x) op e1' e2'
tcExpr (Fn pos (Clause x [] e :| _)) = do
  e' <- tcExpr e
  hole <- newInternalId "$_" ()
  signatureMap . at hole ?= Forall [] (TyTuple 0)
  pure $ Fn (Typed (TyArr (TyTuple 0) (typeOf e')) pos) (Clause (Typed (TyArr (TyTuple 0) (typeOf e')) x) [VarP (Typed (TyTuple 0) pos) hole] e' :| [])
tcExpr (Fn pos cs) = do
  (c' :| cs') <- traverse tcClause cs
  -- パターンの数がすべての節で同じかを検査
  -- tcPatternsでパターンの組み換えを行うので、このタイミングで検査する
  let patNums :: Int = countPatNums c'
  for_ cs' \c -> do
    when (countPatNums c /= patNums) $ do
      let srcFileName = sourceName pos._start
      src <- decodeUtf8 <$> readFileBS srcFileName
      let diag =
            def & \diag ->
              addFile diag srcFileName src & \diag ->
                addReport diag (Err Nothing "The number of patterns in each clause must be the same" (mainDiag c' : map restDiag cs') [])
      printDiagnostic stderr True True 4 defaultStyle diag
      exitFailure
    tell [(pos, typeOf c' :~ typeOf c)]
  pure $ Fn (Typed (typeOf c') pos) (c' :| cs')
  where
    countPatNums (Clause _ ps _) = length ps
    mainDiag (Clause _ ps _) =
      let first = Unsafe.head ps
          last = Unsafe.last ps
       in (rangeToPosition (first ^. range <> last ^. range), This $ render $ "Length of patterns in this clause is" <+> pPrint (length ps))
    restDiag (Clause _ ps _) =
      let first = Unsafe.head ps
          last = Unsafe.last ps
       in (rangeToPosition (first ^. range <> last ^. range), Where $ render $ "Length of patterns in this clause is" <+> pPrint (length ps))
tcExpr (Tuple pos es) = do
  es' <- traverse tcExpr es
  let esType = TyConApp (TyTuple $ length es) $ map typeOf es'
  pure $ Tuple (Typed esType pos) es'
tcExpr (Record pos kvs) = do
  kvs' <- traverse (bitraverse pure tcExpr) kvs
  -- レコードリテラルでは、レコード型をフィールド名から検索する必要はない
  let kvsType = TyRecord $ HashMap.fromList $ map (bimap identity typeOf) kvs'
  pure $ Record (Typed kvsType pos) kvs'
tcExpr (Ann pos e t) = do
  e' <- tcExpr e
  typeRep <- transType t
  tell [(pos, typeOf e' :~ typeRep)]
  pure e'
tcExpr (Seq pos ss) = do
  ss' <- tcStmts ss
  pure $ Seq (Typed (typeOf $ last ss') pos) ss'

tcClause ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m,
    MonadReader RnEnv m
  ) =>
  Clause (Malgo 'Rename) ->
  WriterT [(Range, Constraint)] m (Clause (Malgo 'Infer))
tcClause (Clause pos pats e) = do
  pats' <- tcPatterns pats
  e' <- tcExpr e
  let patTypes = map typeOf pats'
  pure $ Clause (Typed (buildTyArr patTypes (typeOf e')) pos) pats' e'

tcPatterns ::
  ( MonadBind m,
    MonadState TcEnv m,
    MonadFail m,
    MonadIO m
  ) =>
  [Pat (Malgo 'Rename)] ->
  WriterT [(Range, Constraint)] m [Pat (Malgo 'Infer)]
tcPatterns [] = pure []
tcPatterns (VarP x v : ps) = do
  ty <- TyMeta <$> freshVar Nothing
  signatureMap . at v ?= Forall [] ty
  ps' <- tcPatterns ps
  pure $ VarP (Typed ty x) v : ps'
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
  tell [(pos, conType :~ buildTyArr patTypes ty)]
  ps' <- tcPatterns restPs
  pure (ConP (Typed ty pos) con pats' : ps')
tcPatterns (TupleP pos pats : ps) = do
  pats' <- tcPatterns pats
  ps' <- tcPatterns ps
  let patTypes = map typeOf pats'
  pure $ TupleP (Typed (TyConApp (TyTuple (length patTypes)) patTypes) pos) pats' : ps'
tcPatterns (RecordP pos kps : ps) = do
  kps' <- traverseOf (traversed . _2) (\x -> List.head <$> tcPatterns [x]) kps
  ps' <- tcPatterns ps
  let patternType = TyRecord $ HashMap.fromList $ map (bimap identity typeOf) kps'
  pure $ RecordP (Typed patternType pos) kps' : ps'
tcPatterns (UnboxedP pos unboxed : ps) = do
  ps' <- tcPatterns ps
  pure $ UnboxedP (Typed (typeOf unboxed) pos) unboxed : ps'

tcStmts ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadReader RnEnv m,
    MonadIO m
  ) =>
  NonEmpty (Stmt (Malgo 'Rename)) ->
  WriterT [(Range, Constraint)] m (NonEmpty (Stmt (Malgo 'Infer)))
tcStmts = traverse tcStmt

tcStmt ::
  ( MonadState TcEnv m,
    MonadBind m,
    MonadFail m,
    MonadReader RnEnv m,
    MonadIO m
  ) =>
  Stmt (Malgo 'Rename) ->
  WriterT [(Range, Constraint)] m (Stmt (Malgo 'Infer))
tcStmt (NoBind pos e) = NoBind pos <$> tcExpr e
tcStmt (Let pos v e) = do
  e' <- tcExpr e
  signatureMap . at v ?= Forall [] (typeOf e')
  pure $ Let pos v e'

-----------------------------------
-- Translate Type representation --
-----------------------------------

transType :: (MonadState TcEnv m, MonadBind m, MonadReader RnEnv m, MonadIO m) => S.Type (Malgo 'Rename) -> m Type
transType (S.TyApp _ t ts) = TyConApp <$> transType t <*> traverse transType ts
transType (S.TyVar pos v) = lookupType pos v
transType (S.TyCon pos c) = lookupType pos c
transType (S.TyArr _ t1 t2) = TyArr <$> transType t1 <*> transType t2
transType (S.TyTuple _ ts) = TyConApp (TyTuple $ length ts) <$> traverse transType ts
transType (S.TyRecord _ kts) = TyRecord . HashMap.fromList <$> traverseOf (traversed . _2) transType kts

tcType :: S.Type (Malgo 'Rename) -> S.Type (Malgo 'Infer)
tcType (S.TyApp pos t ts) = S.TyApp pos (tcType t) (map tcType ts)
tcType (S.TyVar pos v) = S.TyVar pos v
tcType (S.TyCon pos c) = S.TyCon pos c
tcType (S.TyArr pos t1 t2) = S.TyArr pos (tcType t1) (tcType t2)
tcType (S.TyTuple pos ts) = S.TyTuple pos $ map tcType ts
tcType (S.TyRecord pos kts) = S.TyRecord pos $ over (mapped . _2) tcType kts

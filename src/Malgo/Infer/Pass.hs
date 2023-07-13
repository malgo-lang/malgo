module Malgo.Infer.Pass (infer) where

import Control.Lens (forOf, mapped, to, traverseOf, traversed, view, (^.), _1, _2, _3)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.Extra (anySame)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.State.Static.Shared qualified as S
import Effectful.Writer.Static.Local
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Infer.TcEnv
import Malgo.Infer.TcEnv qualified as TcEnv
import Malgo.Infer.TypeRep
import Malgo.Infer.Unify hiding (lookupVar)
import Malgo.Interface (Interface (..), ModulePathList, loadInterface)
import Malgo.Prelude hiding (Constraint)
import Malgo.Rename.RnEnv (RnEnv (..))
import Malgo.Syntax hiding (Type (..))
import Malgo.Syntax qualified as S
import Malgo.Syntax.Extension

-------------------------------
-- Lookup the value of TcEnv --
-------------------------------

lookupVar ::
  (State TcEnv :> es, IOE :> es) =>
  Range ->
  Id () ->
  Eff es (Scheme Type)
lookupVar pos name =
  gets @TcEnv ((._signatureMap) >>> HashMap.lookup name) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> squotes (pretty name)
    Just scheme -> pure scheme

lookupType :: (State TcEnv :> es, IOE :> es) => Range -> Id () -> Eff es Type
lookupType pos name =
  gets @TcEnv ((._typeDefMap) >>> HashMap.lookup name) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> squotes (pretty name)
    Just TypeDef {..} -> pure _typeConstructor

infer :: (Reader ModulePathList :> es, S.State (HashMap ModuleName Interface) :> es, State Uniq :> es, Reader ModuleName :> es, IOE :> es) => RnEnv -> Module (Malgo Rename) -> Eff es (Module (Malgo Infer), TcEnv)
infer rnEnv (Module name bg) = runReader rnEnv $ do
  tcEnv <- genTcEnv rnEnv
  evalState tcEnv
    $ runTypeUnify
    $ do
      put tcEnv
      bg' <- tcBindGroup bg
      abbrEnv <- gets @TcEnv (._typeSynonymMap)
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
  (Reader ModuleName :> es, State TypeMap :> es, State TcEnv :> es, State Uniq :> es, IOE :> es, S.State (HashMap ModuleName Interface) :> es, Reader ModulePathList :> es) =>
  BindGroup (Malgo Rename) ->
  Eff es (BindGroup (Malgo Infer))
tcBindGroup bindGroup = do
  _imports <- tcImports $ bindGroup ^. imports
  (_typeSynonyms, _dataDefs) <- tcTypeDefinitions (bindGroup ^. typeSynonyms) (bindGroup ^. dataDefs)
  _foreigns <- tcForeigns $ bindGroup ^. foreigns
  _scSigs <- tcScSigs $ bindGroup ^. scSigs
  traverse_ prepareTcScDefs $ bindGroup ^. scDefs
  _scDefs <- tcScDefGroup $ bindGroup ^. scDefs
  pure BindGroup {..}

tcImports :: (State TcEnv :> es, IOE :> es, S.State (HashMap ModuleName Interface) :> es, Reader ModulePathList :> es) => [Import (Malgo Rename)] -> Eff es [Import (Malgo Infer)]
tcImports = traverse tcImport
  where
    tcImport (pos, modName, importList) = do
      interface <- loadInterface modName
      modify \s@TcEnv {..} ->
        s
          { TcEnv._signatureMap = _signatureMap <> interface._signatureMap,
            TcEnv._typeDefMap = _typeDefMap <> interface._typeDefMap,
            TcEnv._typeSynonymMap = _typeSynonymMap <> interface._typeSynonymMap,
            TcEnv._kindCtx = _kindCtx <> interface._kindCtx
          }
      pure (pos, modName, importList)

tcTypeDefinitions ::
  (State TcEnv :> es, State Uniq :> es, Reader ModuleName :> es, IOE :> es) =>
  [TypeSynonym (Malgo Rename)] ->
  [DataDef (Malgo Rename)] ->
  Eff es ([TypeSynonym (Malgo Infer)], [DataDef (Malgo Infer)])
tcTypeDefinitions typeSynonyms dataDefs = do
  -- 相互再帰的な型定義がありうるため、型コンストラクタに対応するTyConを先にすべて生成する
  for_ typeSynonyms \(_, name, params, _) -> do
    let tyCon = name
    modify \s@TcEnv {..} ->
      s
        { TcEnv._kindCtx = insertKind tyCon (buildTyConKind params) _kindCtx,
          TcEnv._typeDefMap = HashMap.insert name (TypeDef (TyCon tyCon) [] []) _typeDefMap
        }
  for_ dataDefs \(_, name, params, _) -> do
    let tyCon = name
    modify \s@TcEnv {..} ->
      s
        { TcEnv._kindCtx = insertKind tyCon (buildTyConKind params) _kindCtx,
          TcEnv._typeDefMap = HashMap.insert name (TypeDef (TyCon tyCon) [] []) _typeDefMap
        }
  typeSynonyms' <- tcTypeSynonyms typeSynonyms
  dataDefs' <- tcDataDefs dataDefs
  pure (typeSynonyms', dataDefs')
  where
    buildTyConKind [] = TYPE
    buildTyConKind (_ : xs) = TyArr TYPE (buildTyConKind xs)

tcTypeSynonyms :: (State TcEnv :> es, IOE :> es, State Uniq :> es, Reader ModuleName :> es) => [TypeSynonym (Malgo Rename)] -> Eff es [TypeSynonym (Malgo Infer)]
tcTypeSynonyms ds =
  for ds \(pos, name, params, typ) -> do
    lookupType pos name >>= \case
      TyCon con -> do
        params' <- for params \p -> newInternalId (idToText p) ()
        zipWithM_
          ( \p p' ->
              modify \s@TcEnv {..} ->
                s
                  { TcEnv._typeDefMap = HashMap.insert p (TypeDef (TyVar p') [] []) _typeDefMap
                  }
          )
          params
          params'
        typ' <- transType typ
        modify \s@TcEnv {..} -> s {TcEnv._typeSynonymMap = HashMap.insert con (params', typ') _typeSynonymMap}

        pure (pos, name, params, tcType typ)
      _ -> error "unreachable: tcTypeSynonyms"

tcDataDefs :: (State TcEnv :> es, IOE :> es, State Uniq :> es, Reader ModuleName :> es) => [DataDef (Malgo Rename)] -> Eff es [DataDef (Malgo Infer)]
tcDataDefs ds = do
  for ds \(pos, name, params, valueCons) -> do
    -- 1. 宣言から、各コンストラクタの型シグネチャを生成する
    name' <- lookupType pos name
    params' <- for params \(_, p) -> newInternalId (idToText p) ()
    zipWithM_
      ( \(_, p) p' ->
          modify
            \s@TcEnv {..} ->
              s
                { TcEnv._typeDefMap = HashMap.insert p (TypeDef (TyVar p') [] []) _typeDefMap
                }
      )
      params
      params'
    (_, valueConsNames, valueConsTypes) <-
      unzip3 <$> forOf (traversed . _3) valueCons \args -> do
        -- 値コンストラクタの型を構築
        args' <- traverse transType args
        pure $ buildTyArr args' (TyConApp name' $ map TyVar params')
    let valueCons' = zip valueConsNames $ map (Forall params') valueConsTypes
    -- signatureMap <>= HashMap.fromList valueCons'
    modify \s@TcEnv {..} ->
      s {TcEnv._signatureMap = _signatureMap <> HashMap.fromList valueCons'}
    -- 2. 環境に登録する
    modify \s@TcEnv {..} ->
      s
        { TcEnv._typeDefMap =
            HashMap.adjust (\t -> t {_typeParameters = params', _valueConstructors = valueCons'}) name _typeDefMap
        }
    pure (pos, name, params, map (second (map tcType)) valueCons)

tcForeigns ::
  ( Reader ModuleName :> es,
    State Uniq :> es,
    State TypeMap :> es,
    State TcEnv :> es,
    IOE :> es
  ) =>
  [Foreign (Malgo Rename)] ->
  Eff es [Foreign (Malgo Infer)]
tcForeigns ds =
  for ds \((pos, raw), name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar $ Just tyVar.name
      modify \s@TcEnv {..} -> s {TcEnv._typeDefMap = HashMap.insert tyVar (TypeDef (TyMeta tv) [] []) _typeDefMap}
    ty' <- transType ty
    scheme@(Forall _ ty') <- generalize pos ty'
    modify \s@TcEnv {..} -> s {TcEnv._signatureMap = HashMap.insert name scheme _signatureMap}
    pure (Typed ty' (pos, raw), name, tcType ty)

tcScSigs :: (State TcEnv :> es, Reader ModuleName :> es, State Uniq :> es, State TypeMap :> es, IOE :> es) => [ScSig (Malgo Rename)] -> Eff es [ScSig (Malgo Infer)]
tcScSigs ds =
  for ds \(pos, name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar $ Just tyVar.name
      modify \s@TcEnv {..} -> s {TcEnv._typeDefMap = HashMap.insert tyVar (TypeDef (TyMeta tv) [] []) _typeDefMap}
    scheme <- generalize pos =<< transType ty
    modify \s@TcEnv {..} -> s {TcEnv._signatureMap = HashMap.insert name scheme _signatureMap}
    pure (pos, name, tcType ty)

prepareTcScDefs :: (State TcEnv :> es, State Uniq :> es, Reader ModuleName :> es) => [ScDef (Malgo Rename)] -> Eff es ()
prepareTcScDefs = traverse_ \(_, name, _) -> do
  mty <- gets @TcEnv $ (._signatureMap) >>> HashMap.lookup name
  case mty of
    Nothing -> do
      ty <- Forall [] . TyMeta <$> freshVar Nothing
      modify \s@TcEnv {..} -> s {TcEnv._signatureMap = HashMap.insert name ty _signatureMap}
    Just _ -> pure ()

tcScDefGroup :: (State TcEnv :> es, State TypeMap :> es, IOE :> es, Reader ModuleName :> es, State Uniq :> es) => [[ScDef (Malgo Rename)]] -> Eff es [[ScDef (Malgo Infer)]]
tcScDefGroup = traverse tcScDefs

tcScDefs :: (State TcEnv :> es, State TypeMap :> es, IOE :> es, Reader ModuleName :> es, State Uniq :> es) => [ScDef (Malgo Rename)] -> Eff es [ScDef (Malgo Infer)]
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
tcScDef :: (State TcEnv :> es, Reader ModuleName :> es, State Uniq :> es, State TypeMap :> es, IOE :> es) => ScDef (Malgo Rename) -> Eff es (ScDef (Malgo Infer))
tcScDef (pos, name, expr) = do
  (expr', wanted) <- runWriter (tcExpr expr)
  nameType <- instantiate pos =<< lookupVar pos name
  let exprType = typeOf expr'
  let constraints = (pos, nameType :~ exprType) : wanted
  solve constraints
  pure (Typed exprType pos, name, expr')

-- | Validate user-declared type signature and add type schemes to environment
validateSignatures ::
  (State TcEnv :> es, IOE :> es) =>
  -- | definitions of mutualy recursive functions
  [ScDef (Malgo 'Infer)] ->
  -- | signatures of mutualy recursive functions
  ([TypeVar], [Type]) ->
  Eff es ()
validateSignatures ds (as, types) = zipWithM_ checkSingle ds types
  where
    -- check single case
    checkSingle (pos, name, _) inferredSchemeType = do
      declaredScheme <- lookupVar pos.value name
      let inferredScheme = Forall as inferredSchemeType
      case declaredScheme of
        -- No explicit signature
        Forall [] (TyMeta _) ->
          -- signatureMap . at name ?= inferredScheme
          modify \s@TcEnv {..} -> s {TcEnv._signatureMap = HashMap.insert name inferredScheme _signatureMap}
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
          abbrEnv <- gets @TcEnv (._typeSynonymMap)
          let Forall _ declaredType = fmap (expandAllTypeSynonym abbrEnv) declaredScheme
          let Forall _ inferredType = fmap (expandAllTypeSynonym abbrEnv) inferredScheme
          case evidenceOfEquiv declaredType inferredType of
            Just evidence
              | anySame $ Map.elems evidence -> errorOn pos.value $ vsep ["Signature too general:", nest 2 ("Declared:" <+> pretty declaredScheme), nest 2 ("Inferred:" <+> pretty inferredScheme)]
              | otherwise ->
                  modify \s@TcEnv {..} -> s {TcEnv._signatureMap = HashMap.insert name declaredScheme _signatureMap}
            Nothing ->
              errorOn pos.value
                $ vsep
                  [ "Signature mismatch:",
                    nest 2 ("Declared:" <+> pretty declaredScheme),
                    nest 2 ("Inferred:" <+> pretty inferredScheme)
                  ]

-- | Which combination of variables should be unification to consider two types as equal?
-- Use in `tcScDefs`.
evidenceOfEquiv ::
  -- | declared type (∀ was stripped, type synonyms are expanded)
  Type ->
  -- | inferred type (∀ was stripped, type synonyms are expanded)
  Type ->
  -- | evidence of equivalence (or no evidence)
  Maybe (Map Type Type)
evidenceOfEquiv (TyMeta v1) (TyMeta v2)
  | v1 == v2 = Just mempty
  | otherwise = Just $ Map.singleton (TyMeta v1) (TyMeta v2)
evidenceOfEquiv (TyVar v1) (TyVar v2)
  | v1 == v2 = Just mempty
  | otherwise = Just $ Map.singleton (TyVar v1) (TyVar v2)
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
  ( Reader ModuleName :> es,
    State Uniq :> es,
    State TypeMap :> es,
    State TcEnv :> es,
    Writer [(Range, Constraint)] :> es,
    IOE :> es
  ) =>
  Expr (Malgo Rename) ->
  Eff es (Expr (Malgo Infer))
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
  modify \s@TcEnv {..} -> s {TcEnv._signatureMap = HashMap.insert hole (Forall [] (TyTuple 0)) _signatureMap}
  pure $ Fn (Typed (TyArr (TyTuple 0) (typeOf e')) pos) (Clause (Typed (TyArr (TyTuple 0) (typeOf e')) x) [VarP (Typed (TyTuple 0) pos) hole] e' :| [])
tcExpr (Fn pos cs) = do
  (c' :| cs') <- traverse tcClause cs
  -- パターンの数がすべての節で同じかを検査
  -- tcClauseでパターンの組み換えを行うので、このタイミングで検査する
  let patNums = countPatNums c'
  for_ cs' \c -> do
    when (countPatNums c /= patNums) do
      errorOn pos "The number of patterns in each clause must be the same"
    tell [(pos, typeOf c' :~ typeOf c)]
  pure $ Fn (Typed (typeOf c') pos) (c' :| cs')
  where
    countPatNums (Clause _ ps _) = length ps
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
  pure $ Seq (Typed (typeOf $ NonEmpty.last ss') pos) ss'

tcClause ::
  ( Reader ModuleName :> es,
    State Uniq :> es,
    State TypeMap :> es,
    State TcEnv :> es,
    Writer [(Range, Constraint)] :> es,
    IOE :> es
  ) =>
  Clause (Malgo Rename) ->
  Eff es (Clause (Malgo Infer))
tcClause (Clause pos pats e) = do
  pats' <- tcPatterns pats
  e' <- tcExpr e
  let patTypes = map typeOf pats'
  pure $ Clause (Typed (buildTyArr patTypes (typeOf e')) pos) pats' e'

tcPatterns :: (State Uniq :> es, Reader ModuleName :> es, State TcEnv :> es, State TypeMap :> es, IOE :> es, Writer [(Range, Constraint)] :> es) => [Pat (Malgo Rename)] -> Eff es [Pat (Malgo Infer)]
tcPatterns [] = pure []
tcPatterns (VarP x v : ps) = do
  ty <- TyMeta <$> freshVar Nothing
  modify \s@TcEnv {..} -> s {TcEnv._signatureMap = HashMap.insert v (Forall [] ty) _signatureMap}
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
  when (not (null morePats) && not (null restPs))
    $ errorOn pos "Invalid Pattern: You may need to put parentheses"
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
  (Reader ModuleName :> es, State Uniq :> es, State TypeMap :> es, State TcEnv :> es, Writer [(Range, Constraint)] :> es, IOE :> es) =>
  NonEmpty (Stmt (Malgo Rename)) ->
  Eff es (NonEmpty (Stmt (Malgo Infer)))
tcStmts = traverse tcStmt

tcStmt :: (Reader ModuleName :> es, State Uniq :> es, State TypeMap :> es, State TcEnv :> es, Writer [(Range, Constraint)] :> es, IOE :> es) => Stmt (Malgo Rename) -> Eff es (Stmt (Malgo Infer))
tcStmt (NoBind pos e) = NoBind pos <$> tcExpr e
tcStmt (Let pos v e) = do
  e' <- tcExpr e
  modify \s@TcEnv {_signatureMap} -> s {TcEnv._signatureMap = HashMap.insert v (Forall [] (typeOf e')) _signatureMap}
  pure $ Let pos v e'

-----------------------------------
-- Translate Type representation --
-----------------------------------

transType ::
  (State TcEnv :> es, IOE :> es) =>
  S.Type (Malgo Rename) ->
  Eff es Type
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

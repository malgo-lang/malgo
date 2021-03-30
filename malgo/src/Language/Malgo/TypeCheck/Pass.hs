{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeCheck.Pass where

import Control.Arrow ((>>>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List.Extra (anySame)
import Data.Maybe (fromJust)
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Interface (loadInterface, signatureMap, typeDefMap)
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import qualified Language.Malgo.Rename.RnEnv as R
import Language.Malgo.Syntax hiding (Type (..), freevars)
import qualified Language.Malgo.Syntax as S
import Language.Malgo.Syntax.Extension
import Language.Malgo.TypeCheck.TcEnv
import Language.Malgo.TypeRep.Static (Rep (..), TypeF)
import qualified Language.Malgo.TypeRep.Static as Static
import Language.Malgo.TypeRep.UTerm
import Language.Malgo.UTerm
import Language.Malgo.Unify hiding (lookupVar)
import Text.Megaparsec (SourcePos)

-------------------------------
-- Lookup the value of TcEnv --
-------------------------------

lookupVar :: (MonadState TcEnv m, MonadMalgo m) => SourcePos -> RnId -> m Scheme
lookupVar pos name =
  use (varEnv . at name) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just scheme -> pure scheme

lookupType :: (MonadState TcEnv m, MonadMalgo m) => SourcePos -> RnTId -> m UType
lookupType pos name =
  preuse (typeEnv . at name . _Just . typeConstructor) >>= \case
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just typ -> pure typ

typeCheck :: (MonadUniq m, MonadMalgo m) => RnEnv -> Module (Malgo 'Rename) -> m (Module (Malgo 'TypeCheck), TcEnv)
typeCheck rnEnv (Module name bg) =
  runTypeUnifyT $ do
    tcEnv <- genTcEnv rnEnv
    (bg', tcEnv') <- runStateT (tcBindGroup bg) tcEnv
    zonkedBg <-
      pure bg'
        >>= traverseOf (scDefs . traversed . traversed . _1 . ann) zonk
        >>= traverseOf (scDefs . traversed . traversed . _3) (walkOn @TypeF @TypeVar zonk)
        >>= traverseOf (foreigns . traversed . _1 . ann) zonk
    zonkedTcEnv <-
      pure tcEnv'
        >>= traverseOf (varEnv . traversed) (walkOn @TypeF @TypeVar zonk)
        >>= traverseOf (typeEnv . traversed . typeConstructor) (walkOn @TypeF @TypeVar zonk)
        >>= traverseOf (typeEnv . traversed . valueConstructors . traversed . _2) (walkOn @TypeF @TypeVar zonk)
    pure (Module name zonkedBg, zonkedTcEnv)

tcBindGroup ::
  ( MonadMalgo m,
    MonadState TcEnv m,
    MonadBind UType m,
    MonadUniq m
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
  ( MonadMalgo m,
    MonadState TcEnv m
  ) =>
  [Import (Malgo 'Rename)] ->
  m [Import (Malgo 'TypeCheck)]
tcImports = traverse tcImport
  where
    tcImport (pos, modName) = do
      interface <- loadInterface modName
      varEnv <>= fmap Static.fromScheme (interface ^. signatureMap)
      typeEnv <>= fmap Static.fromTypeDef (interface ^. typeDefMap)
      pure (pos, modName)

tcTypeDefinitions ::
  ( MonadBind UType m,
    MonadState TcEnv m,
    MonadUniq m,
    MonadMalgo m
  ) =>
  [TypeSynonym (Malgo 'Rename)] ->
  [DataDef (Malgo 'Rename)] ->
  m ([TypeSynonym (Malgo 'TypeCheck)], [DataDef (Malgo 'TypeCheck)])
tcTypeDefinitions typeSynonyms dataDefs = do
  -- 相互再帰的な型定義がありうるため、型コンストラクタに対応するTyConを先にすべて生成する
  for_ typeSynonyms \(x, name, params, _) -> do
    tyCon <- UVar <$> freshVar @UType
    tyConKind <- typeOf tyCon
    solve [With x $ tyConKind :~ buildTyConKind params]
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
  ( MonadBind UType f,
    MonadState TcEnv f,
    MonadMalgo f,
    MonadUniq f
  ) =>
  [TypeSynonym (Malgo 'Rename)] ->
  f [TypeSynonym (Malgo 'TypeCheck)]
tcTypeSynonyms ds =
  for ds \(pos, name, params, typ) -> do
    unless (null params) do
      errorOn pos $
        "Parametized type synonym is not supported"
          $+$ "TODO: add type operator and fix TyTyple and TyLazy's kinding"
    name' <- lookupType pos name
    params' <- traverse (const $ UVar <$> freshVar @UType) params
    nameKind <- typeOf name'
    paramKinds <- traverse typeOf params'
    solve [With pos $ foldr TyArr (TYPE $ Rep BoxedRep) paramKinds :~ nameKind]
    zipWithM_ (\p p' -> typeEnv . at p .= Just (TypeDef p' [] [])) params params'
    transedTyp <- transType typ
    solve [With pos $ foldr (flip TyApp) name' params' :~ transedTyp]
    pure (pos, name, params, tcType typ)

tcDataDefs ::
  ( MonadState TcEnv m,
    MonadMalgo m,
    MonadBind UType m,
    MonadUniq m
  ) =>
  [DataDef (Malgo 'Rename)] ->
  m [DataDef (Malgo 'TypeCheck)]
tcDataDefs ds = do
  bindedTypeVars <- HashSet.unions . map (freevars . view typeConstructor) . HashMap.elems <$> use typeEnv
  for ds \(pos, name, params, valueCons) -> do
    name' <- lookupType pos name
    params' <- traverse (const $ UVar <$> freshVar @UType) params
    nameKind <- typeOf name'
    paramKinds <- traverse typeOf params'
    solve [With pos $ foldr TyArr (TYPE $ Rep BoxedRep) paramKinds :~ nameKind]
    zipWithM_ (\p p' -> typeEnv . at p .= Just (TypeDef p' [] [])) params params'
    (valueConsNames, valueConsTypes) <-
      unzip <$> forOf (traversed . _2) valueCons \args -> do
        -- 値コンストラクタの型を構築
        -- name' <- lookupType pos name
        -- params' <- traverse (lookupType pos) params
        args' <- traverse transType args
        pure $ foldr TyArr (foldr (flip TyApp) name' params') args'
    -- let valueConsNames = map fst valueCons'
    -- let valueConsTypes = map snd valueCons'
    (as, valueConsTypes') <- generalizeMutRecs pos bindedTypeVars valueConsTypes
    let valueCons' = zip valueConsNames valueConsTypes'
    varEnv <>= HashMap.fromList (map (over _2 (Forall as)) valueCons')
    typeEnv . at name %= (_Just . typeParameters .~ map (over idMeta unfreeze) as) . (_Just . valueConstructors .~ valueCons')
    pure (pos, name, params, map (second (map tcType)) valueCons)

tcForeigns ::
  ( MonadMalgo m,
    MonadState TcEnv m,
    MonadBind UType m,
    MonadUniq m
  ) =>
  [Foreign (Malgo 'Rename)] ->
  m [Foreign (Malgo 'TypeCheck)]
tcForeigns ds =
  for ds \((pos, raw), name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar @UType
      typeEnv . at tyVar ?= TypeDef (UVar tv) [] []
    scheme@(Forall _ ty') <- generalize pos mempty =<< transType ty
    varEnv . at name ?= scheme
    pure (With ty' (pos, raw), name, tcType ty)

tcScSigs ::
  ( MonadMalgo m,
    MonadBind UType m,
    MonadState TcEnv m,
    MonadUniq m
  ) =>
  [ScSig (Malgo 'Rename)] ->
  m [ScSig (Malgo 'TypeCheck)]
tcScSigs ds =
  for ds \(pos, name, ty) -> do
    for_ (HashSet.toList $ getTyVars ty) \tyVar -> do
      tv <- freshVar @UType
      typeEnv . at tyVar ?= TypeDef (UVar tv) [] []
    scheme <- generalize pos mempty =<< transType ty
    varEnv . at name ?= scheme
    pure (pos, name, tcType ty)

prepareTcScDefs :: (MonadState TcEnv m, MonadBind UType m) => [ScDef (Malgo 'Rename)] -> m ()
prepareTcScDefs = traverse_ \(_, name, _) -> do
  use (varEnv . at name) >>= \case
    Nothing -> do
      ty <- Forall [] . UVar <$> freshVar @UType
      varEnv . at name ?= ty
    Just _ -> pure ()

tcScDefGroup ::
  ( MonadBind UType m,
    MonadState TcEnv m,
    MonadMalgo m,
    MonadUniq m
  ) =>
  [[ScDef (Malgo 'Rename)]] ->
  m [[ScDef (Malgo 'TypeCheck)]]
tcScDefGroup = traverse tcScDefs

tcScDefs ::
  ( MonadBind UType m,
    MonadState TcEnv m,
    MonadMalgo m,
    MonadUniq m
  ) =>
  [ScDef (Malgo 'Rename)] ->
  m [ScDef (Malgo 'TypeCheck)]
tcScDefs [] = pure []
tcScDefs ds@((pos, _, _) : _) = do
  ds <- for ds \(pos, name, expr) -> do
    (expr', wanted) <- runWriterT (tcExpr expr)
    nameType <- instantiate pos =<< lookupVar pos name
    exprType <- typeOf expr'
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
        declaredType <- instantiate (pos ^. value) declaredScheme
        inferedType <- instantiate (pos ^. value) inferredScheme
        case equiv declaredType inferedType of
          Nothing -> errorOn (pos ^. value) $ "Signature mismatch:" $$ nest 2 ("Declared:" <+> pPrint declaredScheme) $$ nest 2 ("Inferred:" <+> pPrint inferredScheme)
          Just subst
            | anySame $ HashMap.elems subst -> errorOn (pos ^. value) $ "Signature too general:" $$ nest 2 ("Declared:" <+> pPrint declaredScheme) $$ nest 2 ("Inferred:" <+> pPrint inferredScheme)
            | otherwise -> varEnv . at name ?= declaredScheme
  pure ds

tcExpr ::
  ( MonadBind UType m,
    MonadState TcEnv m,
    MonadMalgo m,
    MonadUniq m
  ) =>
  Exp (Malgo 'Rename) ->
  WriterT [With SourcePos (Constraint UType)] m (Exp (Malgo 'TypeCheck))
tcExpr (Var pos v) = do
  vType <- instantiate pos =<< lookupVar pos v
  pure $ Var (With vType pos) v
tcExpr (Con pos c) = do
  cType <- instantiate pos =<< lookupVar pos c
  pure $ Con (With cType pos) c
tcExpr (Unboxed pos u) = do
  uType <- typeOf u
  pure $ Unboxed (With uType pos) u
tcExpr (Apply pos f x) = do
  f' <- tcExpr f
  x' <- tcExpr x
  retType <- UVar <$> freshVar @UType
  fType <- typeOf f'
  xType <- typeOf x'
  tell [With pos $ fType :~ TyArr xType retType]
  pure $ Apply (With retType pos) f' x'
tcExpr (OpApp x@(pos, _) op e1 e2) = do
  e1' <- tcExpr e1
  e2' <- tcExpr e2
  opScheme <- lookupVar pos op
  opType <- instantiate pos opScheme
  retType <- UVar <$> freshVar @UType
  e1Type <- typeOf e1'
  e2Type <- typeOf e2'
  tell [With pos $ opType :~ TyArr e1Type (TyArr e2Type retType)]
  pure $ OpApp (With retType x) op e1' e2'
tcExpr (Fn pos (Clause x [] ss : _)) = do
  ss' <- tcStmts ss
  ssType <- typeOf $ last ss'
  pure $ Fn (With (TyLazy ssType) pos) [Clause (With (TyLazy ssType) x) [] ss']
tcExpr (Fn pos cs) = do
  traverse tcClause cs >>= \case
    (c' : cs') -> do
      c'Type <- typeOf c'
      for_ cs' \c -> do
        cType <- typeOf c
        tell [With pos $ c'Type :~ cType]
      pure $ Fn (With c'Type pos) (c' : cs')
    _ -> bug Unreachable -- Language.Malgo.ParserはsepBy1でFnをパースする
tcExpr (Tuple pos es) = do
  es' <- traverse tcExpr es
  esType <- TyTuple <$> traverse typeOf es'
  pure $ Tuple (With esType pos) es'
tcExpr (Force pos e) = do
  e' <- tcExpr e
  ty <- UVar <$> freshVar @UType
  eType <- typeOf e'
  tell [With pos $ TyLazy ty :~ eType]
  pure $ Force (With ty pos) e'
tcExpr (Parens pos e) = do
  e' <- tcExpr e
  eType <- typeOf e'
  pure $ Parens (With eType pos) e'

tcClause ::
  ( MonadBind UType m,
    MonadState TcEnv m,
    MonadMalgo m,
    MonadUniq m
  ) =>
  Clause (Malgo 'Rename) ->
  WriterT [With SourcePos (Constraint UType)] m (Clause (Malgo 'TypeCheck))
tcClause (Clause pos pats ss) = do
  pats' <- tcPatterns pats
  ss' <- tcStmts ss
  ssType <- typeOf $ last ss'
  patTypes <- traverse typeOf pats'
  pure $ Clause (With (foldr TyArr ssType patTypes) pos) pats' ss'

tcPatterns ::
  ( MonadBind UType m,
    MonadState TcEnv m,
    MonadMalgo m
  ) =>
  [Pat (Malgo 'Rename)] ->
  WriterT [With SourcePos (Constraint UType)] m [Pat (Malgo 'TypeCheck)]
tcPatterns [] = pure []
tcPatterns (VarP x v : ps) = do
  ty <- UVar <$> freshVar @UType
  varEnv . at v ?= Forall [] ty
  ps' <- tcPatterns ps
  pure $ VarP (With ty x) v : ps'
tcPatterns (ConP pos con pats : ps) = do
  conType <- instantiate pos =<< lookupVar pos con
  let (conParams, _) = splitTyArr conType
  -- コンストラクタの型に基づくASTの組み換え
  -- 足りない分を後続のパターン列から補充
  let (morePats, restPs) = splitAt (length conParams - length pats) ps
  -- 足りない分（morePats）を補充した残り（restPs）が空でなければ、
  -- 2引数以上の関数での文法エラー
  when (not (null morePats) && not (null restPs)) $
    errorOn pos "Invalid Pattern: You may need to put parentheses"
  pats' <- tcPatterns (pats <> morePats)
  ty <- UVar <$> freshVar @UType
  patTypes <- traverse typeOf pats'
  tell [With pos $ conType :~ foldr TyArr ty patTypes]
  ps' <- tcPatterns restPs
  pure (ConP (With ty pos) con pats' : ps')
tcPatterns (TupleP pos pats : ps) = do
  pats' <- tcPatterns pats
  ps' <- tcPatterns ps
  patTypes <- traverse typeOf pats'
  pure $ TupleP (With (TyTuple patTypes) pos) pats' : ps'
tcPatterns (UnboxedP pos unboxed : ps) = do
  ps' <- tcPatterns ps
  unboxedType <- typeOf unboxed
  pure $ UnboxedP (With unboxedType pos) unboxed : ps'

splitTyArr :: UType -> ([UType], UType)
splitTyArr (UVar _) = bug Unreachable
splitTyArr (TyArr t1 t2) = let (ps, r) = splitTyArr t2 in (t1 : ps, r)
splitTyArr t = ([], t)

tcStmts ::
  ( MonadMalgo m,
    MonadState TcEnv m,
    MonadBind UType m,
    MonadUniq m
  ) =>
  [Stmt (Malgo 'Rename)] ->
  WriterT [With SourcePos (Constraint UType)] m [Stmt (Malgo 'TypeCheck)]
tcStmts = traverse tcStmt

tcStmt ::
  ( MonadMalgo m,
    MonadState TcEnv m,
    MonadBind UType m,
    MonadUniq m
  ) =>
  Stmt (Malgo 'Rename) ->
  WriterT [With SourcePos (Constraint UType)] m (Stmt (Malgo 'TypeCheck))
tcStmt (NoBind pos e) = NoBind pos <$> tcExpr e
tcStmt (Let pos v e) = do
  env <- use varEnv
  envSet <- traverse (zonk . (\(Forall _ t) -> t)) (HashMap.elems env)
  (e', wanted) <- listen $ tcExpr e
  solve wanted
  -- FIXME: value restriction
  vScheme <- generalize pos (mconcat $ map freevars envSet) =<< typeOf e'
  varEnv . at v ?= vScheme
  pure $ Let pos v e'

-----------------------------------
-- Translate Type representation --
-----------------------------------

transType :: (MonadMalgo m, MonadState TcEnv m, MonadUniq m, MonadBind UType m) => S.Type (Malgo 'Rename) -> m UType
transType (S.TyApp pos t ts) = do
  rnEnv <- use rnEnv
  let ptr_t = fromJust $ findBuiltinType "Ptr#" rnEnv
  case (t, ts) of
    (S.TyCon _ c, [t]) | c == ptr_t -> do
      t' <- transType t
      t'Kind <- typeOf t'
      rep <- UVar . TypeVar <$> newLocalId "r" TyRep
      solve [With pos $ t'Kind :~ TYPE rep]
      pure $ TyPtr t'
    _ -> do
      t' <- transType t
      ts' <- traverse transType ts
      t'Kind <- typeOf t'
      ts'Kinds <- traverse typeOf ts'
      solve [With pos $ foldr TyArr (TYPE $ Rep BoxedRep) ts'Kinds :~ t'Kind]
      foldr (flip TyApp) <$> transType t <*> traverse transType ts
  where
    findBuiltinType :: String -> RnEnv -> Maybe (Id ())
    findBuiltinType x rnEnv = do
      ids <- view (R.typeEnv . at x) rnEnv
      find (view idSort >>> \case WiredIn (ModuleName "Builtin") -> True; _ -> False) ids
transType (S.TyVar pos v) = lookupType pos v
transType (S.TyCon pos c) = lookupType pos c
transType (S.TyArr _ t1 t2) = TyArr <$> transType t1 <*> transType t2
transType (S.TyTuple _ ts) = TyTuple <$> traverse transType ts
transType (S.TyLazy _ t) = TyLazy <$> transType t

tcType :: S.Type (Malgo 'Rename) -> S.Type (Malgo 'TypeCheck)
tcType (S.TyApp pos t ts) = S.TyApp pos (tcType t) (map tcType ts)
tcType (S.TyVar pos v) = S.TyVar pos v
tcType (S.TyCon pos c) = S.TyCon pos c
tcType (S.TyArr pos t1 t2) = S.TyArr pos (tcType t1) (tcType t2)
tcType (S.TyTuple pos ts) = S.TyTuple pos $ map tcType ts
tcType (S.TyLazy pos t) = S.TyLazy pos $ tcType t

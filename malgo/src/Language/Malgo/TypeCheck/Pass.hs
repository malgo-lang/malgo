{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeCheck.Pass (typeCheck, applySubst) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Interface
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import qualified Language.Malgo.Rename.RnEnv as R
import Language.Malgo.Syntax hiding (Type (..))
import qualified Language.Malgo.Syntax as S
import Language.Malgo.Syntax.Extension
import Language.Malgo.Type
import Language.Malgo.TypeCheck.Constraint
import Language.Malgo.TypeCheck.TcEnv hiding (rnEnv)
import qualified Language.Malgo.TypeCheck.TcEnv as TcEnv
import Text.Megaparsec (SourcePos)

-- Entry point
typeCheck :: (MonadUniq m, MonadIO m, MonadMalgo m) => RnEnv -> Module (Malgo 'Rename) -> m (Module (Malgo 'TypeCheck), TcEnv)
typeCheck rnEnv (Module name bg) = do
  tcEnv <- genTcEnv rnEnv
  (bg', tcEnv') <- runStateT (tcBindGroup bg) tcEnv
  pure (Module name bg', tcEnv')

tcBindGroup :: (MonadUniq m, MonadState TcEnv m, MonadIO m, MonadMalgo m) => BindGroup (Malgo 'Rename) -> m (BindGroup (Malgo 'TypeCheck))
tcBindGroup bindGroup = do
  imports' <- tcImports $ bindGroup ^. imports
  dataDefs' <- tcDataDefs $ bindGroup ^. dataDefs
  foreigns' <- tcForeigns $ bindGroup ^. foreigns
  scSigs' <- tcScSigs $ bindGroup ^. scSigs
  traverse_ prepareTcScDefs $ bindGroup ^. scDefs
  scDefs' <- tcScDefGroup $ bindGroup ^. scDefs
  -- zonk TcEnv
  get >>= traverseOf (varEnv . traversed) zonkScheme
    >>= traverseOf (typeEnv . traversed . overTypeDef) zonkType
    >>= put
  foreigns'' <- traverseOf (traversed . _1) (overType zonkType) foreigns'
  scDefs'' <-
    traverseOf (traversed . traversed . _1) (overType zonkType)
      =<< traverseOf (traversed . traversed . _3) (overType zonkType) scDefs'
  pure
    BindGroup
      { _dataDefs = dataDefs',
        _infixs = [],
        _foreigns = foreigns'',
        _scSigs = scSigs',
        _scDefs = scDefs'',
        _imports = imports'
      }

tcImports :: (MonadState TcEnv f, MonadMalgo f, MonadIO f) => [Import (Malgo 'Rename)] -> f [Import (Malgo 'TypeCheck)]
tcImports = traverse tcImport
  where
    tcImport (pos, modName) = do
      interface <- loadInterface modName
      varEnv <>= interface ^. signatureMap
      typeEnv <>= interface ^. typeDefMap
      pure (pos, modName)

tcDataDefs ::
  (MonadState TcEnv m, MonadIO m, MonadUniq m, MonadMalgo m) =>
  [DataDef (Malgo 'Rename)] ->
  m [DataDef (Malgo 'TypeCheck)]
tcDataDefs ds = do
  -- 相互再帰的な型定義がありうるため、型コンストラクタに対応するTyConを先にすべて生成する
  for_ ds $ \(_, name, params, _) ->
    typeEnv . at name <~ Just . simpleTypeDef . TyCon <$> newGlobalId (name ^. idName) (kindof params)
  for ds $ \(pos, name, params, cons) -> do
    for_ params $ \p ->
      typeEnv . at p <~ Just . simpleTypeDef . TyMeta <$> newMetaTv Nothing ""
    cons' <- forOf (traversed . _2) cons $ \args -> do
      -- 値コンストラクタの型を構築
      name' <- lookupType pos name
      params' <- traverse (lookupType pos) params
      args' <- traverse transType args
      pure $ foldr TyArr (foldr (flip TyApp) name' params') args'
    (as, cons'') <- generalizeMutRecs mempty cons'
    varEnv <>= Map.fromList cons''
    typeEnv . at name %= (_Just . qualVars .~ as) . (_Just . union .~ cons')
    pure (pos, name, params, map (second (map tcType)) cons)
  where
    -- 型コンストラクタの引数は必ず a :: Type BoxedRep
    kindof [] = Type BoxedRep
    kindof (_ : xs) = KArr (Type BoxedRep) (kindof xs)

tcForeigns ::
  (MonadUniq m, MonadIO m, MonadState TcEnv m, MonadMalgo m) =>
  [Foreign (Malgo 'Rename)] ->
  m [Foreign (Malgo 'TypeCheck)]
tcForeigns ds =
  for ds $ \(pos, name, ty) -> do
    for_ (Set.toList $ getTyVars ty) $ \tyVar ->
      typeEnv . at tyVar <~ Just . simpleTypeDef . TyMeta <$> newMetaTv Nothing (show $ pPrint tyVar)
    scheme@(Forall _ ty') <- generalize mempty =<< transType ty
    varEnv . at name ?= scheme
    pure (WithType pos ty', name, tcType ty)

tcScSigs ::
  (MonadUniq m, MonadIO m, MonadState TcEnv m, MonadMalgo m) =>
  [ScSig (Malgo 'Rename)] ->
  m [ScSig (Malgo 'TypeCheck)]
tcScSigs ds =
  for ds $ \(pos, name, ty) -> do
    for_ (Set.toList $ getTyVars ty) $ \tyVar ->
      typeEnv . at tyVar <~ Just . simpleTypeDef . TyMeta <$> newMetaTv Nothing (show $ pPrint tyVar)
    scheme <- generalize mempty =<< transType ty
    varEnv . at name ?= scheme
    pure (pos, name, tcType ty)

-- ScSigによる型注釈がないScDefの暫定的な型を生成する
prepareTcScDefs ::
  (MonadState TcEnv m, MonadUniq m, MonadIO m) =>
  [ScDef (Malgo 'Rename)] ->
  m ()
prepareTcScDefs ds = for_ ds $ \(_, name, _) -> do
  mscheme <- use $ varEnv . at name
  case mscheme of
    Nothing -> varEnv . at name <~ Just . Forall [] . TyMeta <$> newMetaTv Nothing ""
    Just _ -> pure mempty

tcScDefGroup ::
  (MonadState TcEnv f, MonadUniq f, MonadIO f, MonadMalgo f) =>
  [[ScDef (Malgo 'Rename)]] ->
  f [[ScDef (Malgo 'TypeCheck)]]
tcScDefGroup = traverse tcScDefs

tcScDefs ::
  (MonadState TcEnv m, MonadUniq m, MonadIO m, MonadMalgo m) =>
  [ScDef (Malgo 'Rename)] ->
  m [ScDef (Malgo 'TypeCheck)]
tcScDefs ds = do
  (ds', nts) <- mapAndUnzipM ?? ds $ \(pos, name, expr) -> do
    (expr', wanted) <- runWriterT (tcExpr expr)
    ty <- instantiate True =<< lookupVar pos name
    solve $ eqCons pos ty (expr' ^. toType) : wanted
    pure ((WithType pos ty, name, expr'), (name, ty))
  (_, nts') <- generalizeMutRecs mempty nts
  varEnv %= (Map.fromList nts' <>)
  -- prepareTcScDefsで定義されたvarEnvを更新したい
  -- varEnv <>= Map.fromList nts' では定義が更新されない
  pure ds'

tcExpr ::
  (MonadState TcEnv m, MonadUniq m, MonadIO m, MonadMalgo m) =>
  Exp (Malgo 'Rename) ->
  WriterT [WithPos] m (Exp (Malgo 'TypeCheck))
tcExpr (Var pos v) = do
  vType <- instantiate False =<< lookupVar pos v
  pure $ Var (WithType pos vType) v
tcExpr (Con pos c) = do
  cType <- instantiate False =<< lookupVar pos c
  pure $ Con (WithType pos cType) c
tcExpr (Unboxed pos u) = pure $ Unboxed (WithType pos $ u ^. toType) u
tcExpr (S.Boxed _ _) = bug Unreachable -- RenameでApplyに変形されている
tcExpr (Apply pos f x) = do
  f' <- tcExpr f
  x' <- tcExpr x
  retType <- TyMeta <$> newMetaTv Nothing ""
  tell [eqCons pos (f' ^. toType) (TyArr (x' ^. toType) retType)]
  pure $ Apply (WithType pos retType) f' x'
tcExpr (OpApp x@(pos, _) op e1 e2) = do
  e1' <- tcExpr e1
  e2' <- tcExpr e2
  opScheme <- lookupVar pos op
  opType <- instantiate False opScheme
  retType <- TyMeta <$> newMetaTv Nothing ""
  tell [eqCons pos opType (TyArr (e1' ^. toType) $ TyArr (e2' ^. toType) retType)]
  pure $ OpApp (WithType x retType) op e1' e2'
tcExpr (Fn pos (Clause x [] ss : _)) = do
  ss' <- tcStmts ss
  pure $
    Fn
      (WithType pos (TyLazy $ last ss' ^. toType))
      [Clause (WithType x (TyLazy $ last ss' ^. toType)) [] ss']
tcExpr (Fn pos cs) = do
  cs' <- traverse tcClause cs
  case cs' of
    (c' : cs') -> do
      tell $ map (eqCons pos (c' ^. toType) . view toType) cs'
      pure $ Fn (WithType pos (c' ^. toType)) (c' : cs')
    _ -> bug Unreachable
tcExpr (Tuple pos es) = do
  es' <- traverse tcExpr es
  pure $ Tuple (WithType pos (TyTuple (map (view toType) es'))) es'
tcExpr (Force pos e) = do
  e' <- tcExpr e
  ty <- TyMeta <$> newMetaTv Nothing ""
  tell [eqCons pos (TyLazy ty) (e' ^. toType)]
  pure $ Force (WithType pos ty) e'
tcExpr (Parens pos e) = do
  e' <- tcExpr e
  pure $ Parens (WithType pos (e' ^. toType)) e'

tcClause :: (MonadState TcEnv m, MonadIO m, MonadUniq m, MonadMalgo m) => Clause (Malgo 'Rename) -> WriterT [WithPos] m (Clause (Malgo 'TypeCheck))
tcClause (Clause pos pats ss) = do
  pats' <- tcPatterns pats
  ss' <- tcStmts ss
  pure $ Clause (WithType pos (foldr (TyArr . view toType) (last ss' ^. toType) pats')) pats' ss'

tcStmts :: (MonadState TcEnv m, MonadIO m, MonadUniq m, MonadMalgo m) => [Stmt (Malgo 'Rename)] -> WriterT [WithPos] m [Stmt (Malgo 'TypeCheck)]
tcStmts [] = pure []
tcStmts (NoBind pos e : ss) = do
  e' <- tcExpr e
  ss' <- tcStmts ss
  pure $ NoBind pos e' : ss'
tcStmts (Let pos v e : ss) = do
  env <- get
  (e', wanted) <- listen $ tcExpr e
  solve wanted
  -- FIXME: value restriction
  vScheme <- generalize env (e' ^. toType)
  varEnv . at v ?= vScheme
  ss' <- tcStmts ss
  pure $ Let pos v e' : ss'

tcPatterns :: (MonadState TcEnv m, MonadIO m, MonadUniq m, MonadMalgo m) => [Pat (Malgo 'Rename)] -> WriterT [WithPos] m [Pat (Malgo 'TypeCheck)]
tcPatterns [] = pure []
tcPatterns (VarP x v : ps) = do
  ty <- TyMeta <$> newMetaTv Nothing ""
  varEnv . at v ?= Forall [] ty
  ps' <- tcPatterns ps
  pure (VarP (WithType x ty) v : ps')
tcPatterns (ConP pos con pats : ps) = do
  conType <- instantiate False =<< lookupVar pos con
  let (conParams, _) = splitTyArr conType
  -- コンストラクタの型に基づくASTの組み換え
  -- 足りない分を後続のパターン列から補充
  let (morePats, restPs) = splitAt (length conParams - length pats) ps
  -- 足りない分（morePats）を補充した残り（restPs）が空でなければ、
  -- 2引数以上の関数での文法エラー
  when (not (null morePats) && not (null restPs)) $
    errorOn pos "Invalid Pattern: You may need to put parentheses"
  pats' <- tcPatterns (pats <> morePats)
  ty <- TyMeta <$> newMetaTv Nothing ""
  tell [eqCons pos conType (foldr (TyArr . view toType) ty pats')]
  ps' <- tcPatterns restPs
  pure (ConP (WithType pos ty) con pats' : ps')
tcPatterns (TupleP pos pats : ps) = do
  pats' <- tcPatterns pats
  ps' <- tcPatterns ps
  pure (TupleP (WithType pos (TyTuple $ map (view toType) pats')) pats' : ps')
tcPatterns (UnboxedP pos unboxed : cs) = do
  ps <- tcPatterns cs
  pure (UnboxedP (WithType pos (unboxed ^. toType)) unboxed : ps)

-----------------------------------
-- Translate Type representation --
-----------------------------------

transType :: (MonadState TcEnv m, MonadIO m, MonadMalgo m) => S.Type (Malgo 'Rename) -> m Type
transType (S.TyApp pos t ts) = do
  rnEnv <- use TcEnv.rnEnv
  let ptr_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Ptr#" (view R.typeEnv rnEnv)
  case t of
    S.TyCon _ c | c == ptr_t ->
      case ts of
        [t] -> do
          t' <- transType t
          pure $ TyPtr t'
        _ -> errorOn pos "Invalid type arguments for Ptr#"
    _ -> foldr (flip TyApp) <$> transType t <*> traverse transType ts
transType (S.TyVar pos v) = lookupType pos v
transType (S.TyCon pos c) = do
  rnEnv <- use TcEnv.rnEnv
  -- lookup RnTId of primitive types
  let int32_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Int32#" (view R.typeEnv rnEnv)
  let int64_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Int64#" (view R.typeEnv rnEnv)
  let float_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Float#" (view R.typeEnv rnEnv)
  let double_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Double#" (view R.typeEnv rnEnv)
  let char_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Char#" (view R.typeEnv rnEnv)
  let string_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "String#" (view R.typeEnv rnEnv)
  if
      | c == int32_t -> pure $ TyPrim Int32T
      | c == int64_t -> pure $ TyPrim Int64T
      | c == float_t -> pure $ TyPrim FloatT
      | c == double_t -> pure $ TyPrim DoubleT
      | c == char_t -> pure $ TyPrim CharT
      | c == string_t -> pure $ TyPrim StringT
      | otherwise -> lookupType pos c
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

-------------------------------
-- Lookup the value of TcEnv --
-------------------------------

lookupType :: (HasCallStack, MonadState TcEnv m, MonadMalgo m, MonadIO m) => SourcePos -> RnTId -> m Type
lookupType pos name = do
  mtype <- preuse $ typeEnv . at name . _Just . constructor
  case mtype of
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just typ -> pure typ

lookupVar :: (HasCallStack, MonadState TcEnv m, MonadMalgo m, MonadIO m) => SourcePos -> RnId -> m Scheme
lookupVar pos name = do
  mscheme <- use $ varEnv . at name
  case mscheme of
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just scheme -> pure scheme

--------------------------------
-- Generalize and Instantiate --
--------------------------------

-- 型内の自由変数を取り出し、抽象化する
generalize :: (MonadIO m, MonadUniq m) => TcEnv -> Type -> m Scheme
generalize env t = do
  fvs <- toList <$> freeMetaTvs env t
  as <- zipWithM (\tv nameChar -> do k <- fromMaybe (Type BoxedRep) <$> readIORef (_metaTvKind tv); newLocalId [nameChar] k) fvs ['a' ..]
  zipWithM_ writeMetaTv fvs (map TyVar as)
  Forall as <$> zonkType t

generalizeMutRecs :: (MonadIO m, MonadUniq m) => TcEnv -> [(TcId, Type)] -> m ([Id Kind], [(TcId, Scheme)])
generalizeMutRecs env nts = do
  fvs <- toList . mconcat <$> traverse (freeMetaTvs env <=< zonkType . view _2) nts
  as <- zipWithM (\tv nameChar -> do k <- fromMaybe (Type BoxedRep) <$> readIORef (_metaTvKind tv); newLocalId [nameChar] k) fvs ['a' ..]
  zipWithM_ writeMetaTv fvs (map TyVar as)
  (as,) <$> traverseOf (traversed . _2) (fmap (Forall as) . zonkType) nts

freeMetaTvs :: MonadIO m => TcEnv -> Type -> m (Set MetaTv)
freeMetaTvs env t = do
  env' <- traverse zonkScheme (view varEnv env)
  t' <- zonkType t
  pure $ metaTvs t' Set.\\ foldMap metaTvsScheme env'

-- 型を具体化する
instantiate :: (MonadUniq m, MonadIO m) => Bool -> Scheme -> m Type
instantiate isRigid (Forall as t) = do
  vs <-
    traverse
      ( \a -> do
          mka <- kind a
          TyMeta <$> newMetaTv mka (if isRigid then show $ pPrint a else "")
      )
      as
  applySubst (Map.fromList $ zip as vs) <$> zonkType t

applySubst :: Map TyVar Type -> Type -> Type
applySubst subst (TyVar v) = fromMaybe (TyVar v) $ Map.lookup v subst
applySubst subst (TyApp t1 t2) = TyApp (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyArr t1 t2) = TyArr (applySubst subst t1) (applySubst subst t2)
applySubst subst (TyTuple ts) = TyTuple $ map (applySubst subst) ts
applySubst subst (TyLazy t) = TyLazy $ applySubst subst t
applySubst subst (TyPtr t) = TyPtr $ applySubst subst t
applySubst _ t = t
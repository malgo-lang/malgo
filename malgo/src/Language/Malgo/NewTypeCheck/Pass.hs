{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.Malgo.NewTypeCheck.Pass (typeCheck) where

import Data.Fix
import qualified Data.Map as Map
import qualified Data.Set as Set
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Interface (loadInterface, signatureMap, typeDefMap)
import Language.Malgo.NewTypeCheck.TcEnv
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import qualified Language.Malgo.Rename.RnEnv as R
import Language.Malgo.Syntax hiding (Type (..), freevars)
import qualified Language.Malgo.Syntax as S
import Language.Malgo.Syntax.Extension
import Language.Malgo.TypeRep.Static (IsKind (fromKind, safeToKind), IsType (fromType, safeToType), IsTypeDef (safeToTypeDef), PrimT (..), Rep (..))
import qualified Language.Malgo.TypeRep.Static as Static
import Language.Malgo.TypeRep.UTerm
import Language.Malgo.Unify hiding (lookupVar)
import Text.Megaparsec (SourcePos)

-------------------------------
-- Lookup the value of TcEnv --
-------------------------------

lookupVar :: (MonadState TcEnv m, MonadMalgo m, MonadIO m) => SourcePos -> RnId -> m (Scheme UKind)
lookupVar pos name = do
  mscheme <- use $ varEnv . at name
  case mscheme of
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just scheme -> pure scheme

lookupType :: (MonadState TcEnv m, MonadMalgo m, MonadIO m) => SourcePos -> Id ModuleName -> m UType
lookupType pos name = do
  mtype <- preuse $ typeEnv . at name . _Just . typeConstructor
  case mtype of
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just typ -> pure typ

typeCheck :: (MonadUniq m, MonadMalgo m, MonadIO m) => RnEnv -> Module (Malgo 'Rename) -> m (Module (Malgo 'NewTypeCheck), TcEnv)
typeCheck rnEnv (Module name bg) = runKindUnifyT $
  runTypeUnifyT $ do
    let tcEnv = TcEnv mempty mempty rnEnv
    (bg', tcEnv') <- runStateT (tcBindGroup bg) tcEnv
    zonkedBg <-
      pure bg'
        >>= traverseOf (scDefs . traversed . traversed . _1 . ann) (zonkUTerm >=> walkOn @KindF @KindVar (liftKindUnifyT . (zonkUTerm >=> bindUnknownToBoxed)))
        >>= traverseOf (scDefs . traversed . traversed . _3) (walkOn @(TypeF UKind) @(TypeVar UKind) $ zonkUTerm >=> walkOn @KindF @KindVar (liftKindUnifyT . (zonkUTerm >=> bindUnknownToBoxed)))
        >>= traverseOf (foreigns . traversed . _1 . ann) (zonkUTerm >=> walkOn @KindF @KindVar (liftKindUnifyT . (zonkUTerm >=> bindUnknownToBoxed)))
    zonkedTcEnv <-
      pure tcEnv'
        >>= traverseOf (varEnv . traversed) (walkOn @(TypeF UKind) @(TypeVar UKind) zonkUTerm >=> walkOn @KindF @KindVar (liftKindUnifyT . (zonkUTerm >=> bindUnknownToBoxed)))
        >>= traverseOf (typeEnv . traversed . typeConstructor) (walkOn @(TypeF UKind) @(TypeVar UKind) zonkUTerm >=> walkOn @KindF @KindVar (liftKindUnifyT . (zonkUTerm >=> bindUnknownToBoxed)))
        >>= traverseOf (typeEnv . traversed . typeParameters . traversed . idMeta) (liftKindUnifyT . (zonkUTerm >=> bindUnknownToBoxed))
        >>= traverseOf (typeEnv . traversed . valueConstructors . traversed . _2) (walkOn @(TypeF UKind) @(TypeVar UKind) zonkUTerm >=> walkOn @KindF @KindVar (liftKindUnifyT . (zonkUTerm >=> bindUnknownToBoxed)))
    pure (Module name zonkedBg, zonkedTcEnv)

tcBindGroup :: (MonadIO m, MonadUniq m, MonadMalgo m) => BindGroup (Malgo 'Rename) -> StateT TcEnv (TypeUnifyT m) (BindGroup (Malgo 'NewTypeCheck))
tcBindGroup bindGroup = do
  imports' <- tcImports $ bindGroup ^. imports
  dataDefs' <- tcDataDefs $ bindGroup ^. dataDefs
  foreigns' <- tcForeigns $ bindGroup ^. foreigns
  scSigs' <- tcScSigs $ bindGroup ^. scSigs
  traverse_ prepareTcScDefs $ bindGroup ^. scDefs
  scDefs' <- tcScDefGroup $ bindGroup ^. scDefs
  pure
    BindGroup
      { _dataDefs = dataDefs',
        _foreigns = foreigns',
        _scSigs = scSigs',
        _scDefs = scDefs',
        _imports = imports'
      }

tcImports :: (MonadMalgo m, MonadIO m, MonadState TcEnv m) => [Import (Malgo 'Rename)] -> m [Import (Malgo 'NewTypeCheck)]
tcImports = traverse tcImport
  where
    tcImport (pos, modName) = do
      interface <- loadInterface modName
      varEnv <>= fmap Static.fromScheme (interface ^. signatureMap)
      typeEnv <>= fmap Static.fromTypeDef (interface ^. typeDefMap)
      pure (pos, modName)

tcDataDefs :: (MonadIO m, MonadUniq m, MonadMalgo m) => [DataDef (Malgo 'Rename)] -> StateT TcEnv (TypeUnifyT m) [DataDef (Malgo 'NewTypeCheck)]
tcDataDefs ds = do
  -- 相互再帰的な型定義がありうるため、型コンストラクタに対応するTyConを先にすべて生成する
  for_ ds $ \(_, name, params, _) -> do
    tyCon <- UTerm . TyCon <$> newGlobalId (name ^. idName) (buildTyConKind params)
    typeEnv . at name .= Just (TypeDef tyCon [] [])
  for ds $ \(pos, name, params, valueCons) -> do
    name' <- lookupType pos name
    params' <- traverse (const $ UVar <$> freshVar) params
    lift $ liftKindUnifyT $ solve [With pos $ foldr ((\l r -> UTerm $ KArr l r) . kindOf) (UTerm $ Type BoxedRep) params' :~ kindOf name']
    zipWithM_ (\p p' -> typeEnv . at p .= Just (TypeDef p' [] [])) params params'
    valueCons' <- forOf (traversed . _2) valueCons $ \args -> do
      -- 値コンストラクタの型を構築
      -- name' <- lookupType pos name
      -- params' <- traverse (lookupType pos) params
      args' <- traverse transType args
      pure $ foldr (\l r -> UTerm $ TyArr l r) (foldr (\l r -> UTerm $ TyApp r l) name' params') args'
    let valueConsNames = map fst valueCons'
    let valueConsTypes = map snd valueCons'
    (as, valueConsTypes') <- generalizeMutRecs pos mempty valueConsTypes
    let valueCons'' = zip valueConsNames valueConsTypes'
    varEnv <>= Map.fromList (map (over _2 (Forall as)) valueCons'')
    typeEnv . at name %= (_Just . typeParameters .~ as) . (_Just . valueConstructors .~ valueCons'')
    pure (pos, name, params, map (second (map tcType)) valueCons)
  where
    buildTyConKind [] = UTerm $ Type BoxedRep
    buildTyConKind (_ : xs) = UTerm $ KArr (UTerm $ Type BoxedRep) (buildTyConKind xs)

tcForeigns :: (MonadMalgo m, MonadState TcEnv m, MonadBind (TypeF UKind) (TypeVar UKind) m, MonadIO m, MonadUniq m) => [Foreign (Malgo 'Rename)] -> m [Foreign (Malgo 'NewTypeCheck)]
tcForeigns ds =
  for ds $ \(pos, name, ty) -> do
    for_ (Set.toList $ getTyVars ty) $ \tyVar -> do
      tv <- freshVar
      let tyVar' = UVar $ tv {typeVarRigidName = show $ pPrint tyVar}
      typeEnv . at tyVar ?= TypeDef tyVar' [] []
    scheme@(Forall _ ty') <- generalize pos mempty =<< transType ty
    varEnv . at name ?= scheme
    pure (With ty' pos, name, tcType ty)

tcScSigs :: (MonadMalgo m, MonadBind (TypeF UKind) (TypeVar UKind) m, MonadState TcEnv m, MonadIO m, MonadUniq m) => [ScSig (Malgo 'Rename)] -> m [ScSig (Malgo 'NewTypeCheck)]
tcScSigs ds =
  for ds $ \(pos, name, ty) -> do
    for_ (Set.toList $ getTyVars ty) $ \tyVar -> do
      tv <- freshVar
      let tyVar' = UVar $ tv {typeVarRigidName = show $ pPrint tyVar}
      typeEnv . at tyVar ?= TypeDef tyVar' [] []
    scheme <- generalize pos mempty =<< transType ty
    varEnv . at name ?= scheme
    pure (pos, name, tcType ty)

prepareTcScDefs :: (MonadState TcEnv m, MonadBind (TypeF UKind) (TypeVar UKind) m) => [ScDef (Malgo 'Rename)] -> m ()
prepareTcScDefs = traverse_ \(_, name, _) -> do
  mscheme <- use $ varEnv . at name
  case mscheme of
    Nothing -> do
      ty <- Forall [] . UVar <$> freshVar
      varEnv . at name ?= ty
    Just _ -> pure ()

tcScDefGroup :: (MonadBind (TypeF UKind) (TypeVar UKind) m, MonadState TcEnv m, MonadMalgo m, MonadIO m, MonadUniq m) => [[ScDef (Malgo 'Rename)]] -> m [[ScDef (Malgo 'NewTypeCheck)]]
tcScDefGroup = traverse tcScDefs

tcScDefs :: (MonadBind (TypeF UKind) (TypeVar UKind) m, MonadState TcEnv m, MonadMalgo m, MonadIO m, MonadUniq m) => [ScDef (Malgo 'Rename)] -> m [ScDef (Malgo 'NewTypeCheck)]
tcScDefs [] = pure []
tcScDefs ds@((pos, _, _) : _) = do
  (ds', nts) <- mapAndUnzipM ?? ds $ \(pos, name, expr) -> do
    (expr', wanted) <- runWriterT (tcExpr expr)
    ty <- instantiate True =<< lookupVar pos name
    let constraints = With pos (ty :~ typeOf expr') : wanted
    solve constraints
    pure ((With ty pos, name, expr'), (name, ty))
  let names = map fst nts
  let types = map snd nts
  zonkedTypes <- traverse zonkUTerm types
  (as, types') <- generalizeMutRecs pos mempty zonkedTypes
  varEnv %= (Map.fromList (zip names $ map (Forall as) types') <>)
  pure ds'

tcExpr :: (MonadBind (TypeF UKind) (TypeVar UKind) m, MonadState TcEnv m, MonadMalgo m, MonadIO m, MonadUniq m) => Exp (Malgo 'Rename) -> WriterT [WithMeta SourcePos (TypeF UKind) (TypeVar UKind)] m (Exp (Malgo 'NewTypeCheck))
tcExpr (Var pos v) = do
  vType <- instantiate False =<< lookupVar pos v
  pure $ Var (With vType pos) v
tcExpr (Con pos c) = do
  cType <- instantiate False =<< lookupVar pos c
  pure $ Con (With cType pos) c
tcExpr (Unboxed pos u) = pure $ Unboxed (With (typeOf u) pos) u
tcExpr (Apply pos f x) = do
  f' <- tcExpr f
  x' <- tcExpr x
  retType <- UVar <$> freshVar
  tell [With pos $ typeOf f' :~ UTerm (TyArr (typeOf x') retType)]
  pure $ Apply (With retType pos) f' x'
tcExpr (OpApp x@(pos, _) op e1 e2) = do
  e1' <- tcExpr e1
  e2' <- tcExpr e2
  opScheme <- lookupVar pos op
  opType <- instantiate False opScheme
  retType <- UVar <$> freshVar
  tell [With pos $ opType :~ UTerm (TyArr (typeOf e1') $ UTerm $ TyArr (typeOf e2') retType)]
  pure $ OpApp (With retType x) op e1' e2'
tcExpr (Fn pos (Clause x [] ss : _)) = do
  ss' <- tcStmts ss
  pure $ Fn (With (UTerm $ TyLazy $ typeOf $ last ss') pos) [Clause (With (UTerm $ TyLazy $ typeOf $ last ss') x) [] ss']
tcExpr (Fn pos cs) = do
  cs' <- traverse tcClause cs
  case cs' of
    (c' : cs') -> do
      tell $ map (\c -> With pos $ typeOf c' :~ typeOf c) cs'
      pure $ Fn (With (typeOf c') pos) (c' : cs')
    _ -> bug Unreachable
tcExpr (Tuple pos es) = do
  es' <- traverse tcExpr es
  pure $ Tuple (With (UTerm $ TyTuple (map typeOf es')) pos) es'
tcExpr (Force pos e) = do
  e' <- tcExpr e
  ty <- UVar <$> freshVar
  tell [With pos $ UTerm (TyLazy ty) :~ typeOf e']
  pure $ Force (With ty pos) e'
tcExpr (Parens pos e) = do
  e' <- tcExpr e
  pure $ Parens (With (typeOf e') pos) e'

tcClause :: (MonadBind (TypeF UKind) (TypeVar UKind) m, MonadState TcEnv m, MonadMalgo m, MonadIO m, MonadUniq m) => Clause (Malgo 'Rename) -> WriterT [WithMeta SourcePos (TypeF UKind) (TypeVar UKind)] m (Clause (Malgo 'NewTypeCheck))
tcClause (Clause pos pats ss) = do
  pats' <- tcPatterns pats
  ss' <- tcStmts ss
  pure $ Clause (With (foldr (\l r -> UTerm $ TyArr (typeOf l) r) (typeOf $ last ss') pats') pos) pats' ss'

tcPatterns :: (MonadBind (TypeF UKind) (TypeVar UKind) m, MonadState TcEnv m, MonadIO m, MonadMalgo m) => [Pat (Malgo 'Rename)] -> WriterT [WithMeta SourcePos (TypeF UKind) (TypeVar UKind)] m [Pat (Malgo 'NewTypeCheck)]
tcPatterns [] = pure []
tcPatterns (VarP x v : ps) = do
  ty <- UVar <$> freshVar
  varEnv . at v ?= Forall [] ty
  ps' <- tcPatterns ps
  pure $ VarP (With ty x) v : ps'
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
  ty <- UVar <$> freshVar
  tell [With pos $ conType :~ foldr (\l r -> UTerm $ TyArr (typeOf l) r) ty pats']
  ps' <- tcPatterns restPs
  pure (ConP (With ty pos) con pats' : ps')
tcPatterns (TupleP pos pats : ps) = do
  pats' <- tcPatterns pats
  ps' <- tcPatterns ps
  pure $ TupleP (With (UTerm $ TyTuple $ map typeOf pats') pos) pats' : ps'
tcPatterns (UnboxedP pos unboxed : ps) = do
  ps' <- tcPatterns ps
  pure $ UnboxedP (With (typeOf unboxed) pos) unboxed : ps'

splitTyArr :: UType -> ([UType], UType)
splitTyArr (UVar _) = bug Unreachable
splitTyArr (UTerm (TyArr t1 t2)) = let (ps, r) = splitTyArr t2 in (t1 : ps, r)
splitTyArr t = ([], t)

tcStmts :: (MonadIO m, MonadMalgo m, MonadState TcEnv m, MonadBind (TypeF UKind) (TypeVar UKind) m, MonadUniq m) => [Stmt (Malgo 'Rename)] -> WriterT [WithMeta SourcePos (TypeF UKind) (TypeVar UKind)] m [Stmt (Malgo 'NewTypeCheck)]
tcStmts = traverse tcStmt

tcStmt :: (MonadIO m, MonadMalgo m, MonadState TcEnv m, MonadBind (TypeF UKind) (TypeVar UKind) m, MonadUniq m) => Stmt (Malgo 'Rename) -> WriterT [WithMeta SourcePos (TypeF UKind) (TypeVar UKind)] m (Stmt (Malgo 'NewTypeCheck))
tcStmt (NoBind pos e) = NoBind pos <$> tcExpr e
tcStmt (Let pos v e) = do
  env <- use varEnv
  envSet <- traverse (zonkUTerm . (\(Forall _ t) -> t)) (Map.elems env)
  (e', wanted) <- listen $ tcExpr e
  solve wanted
  -- FIXME: value restriction
  vScheme <- generalize pos (mconcat $ map freevars envSet) (typeOf e')
  varEnv . at v ?= vScheme
  pure $ Let pos v e'

-----------------------------------
-- Translate Type representation --
-----------------------------------

transType :: (MonadMalgo m, MonadState TcEnv m, MonadIO m) => S.Type (Malgo 'Rename) -> m UType
transType (S.TyApp _ t ts) = do
  rnEnv <- use rnEnv
  let ptr_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Ptr#" (view R.typeEnv rnEnv)
  case (t, ts) of
    (S.TyCon _ c, [t]) | c == ptr_t -> do
      t' <- transType t
      pure $ UTerm $ TyPtr t'
    _ -> foldr (\l r -> UTerm $ TyApp r l) <$> transType t <*> traverse transType ts
transType (S.TyVar pos v) = lookupType pos v
transType (S.TyCon pos c) = do
  rnEnv <- use rnEnv
  -- lookup RnTId of primitive types
  let int32_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Int32#" (view R.typeEnv rnEnv)
  let int64_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Int64#" (view R.typeEnv rnEnv)
  let float_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Float#" (view R.typeEnv rnEnv)
  let double_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Double#" (view R.typeEnv rnEnv)
  let char_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Char#" (view R.typeEnv rnEnv)
  let string_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "String#" (view R.typeEnv rnEnv)
  if
      | c == int32_t -> pure $ UTerm $ TyPrim Int32T
      | c == int64_t -> pure $ UTerm $ TyPrim Int64T
      | c == float_t -> pure $ UTerm $ TyPrim FloatT
      | c == double_t -> pure $ UTerm $ TyPrim DoubleT
      | c == char_t -> pure $ UTerm $ TyPrim CharT
      | c == string_t -> pure $ UTerm $ TyPrim StringT
      | otherwise -> lookupType pos c
transType (S.TyArr _ t1 t2) = UTerm <$> (TyArr <$> transType t1 <*> transType t2)
transType (S.TyTuple _ ts) = UTerm <$> (TyTuple <$> traverse transType ts)
transType (S.TyLazy _ t) = UTerm <$> (TyLazy <$> transType t)

tcType :: S.Type (Malgo 'Rename) -> S.Type (Malgo 'NewTypeCheck)
tcType (S.TyApp pos t ts) = S.TyApp pos (tcType t) (map tcType ts)
tcType (S.TyVar pos v) = S.TyVar pos v
tcType (S.TyCon pos c) = S.TyCon pos c
tcType (S.TyArr pos t1 t2) = S.TyArr pos (tcType t1) (tcType t2)
tcType (S.TyTuple pos ts) = S.TyTuple pos $ map tcType ts
tcType (S.TyLazy pos t) = S.TyLazy pos $ tcType t

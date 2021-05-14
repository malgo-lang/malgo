{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | 名前解決
module Language.Malgo.Rename.Pass where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (intersect)
import Data.List.Extra (anySame, disjoint)
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Interface
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv
import Language.Malgo.Syntax
import Language.Malgo.Syntax.Extension
import System.IO (hPrint, stderr)
import Text.Megaparsec.Pos (SourcePos)

data RenameApp = RenameApp {_renameMalgoOpt :: Opt, _renameRnEnv :: RnEnv}

instance HasOpt RenameApp where
  malgoOpt = lens _renameMalgoOpt (\x y -> x {_renameMalgoOpt = y})

instance HasRnEnv RenameApp where
  rnEnv = lens _renameRnEnv (\x y -> x {_renameRnEnv = y})

rename :: (MonadUniq m, MonadReader env m, HasOpt env, MonadIO m) => RnEnv -> Module (Malgo 'Parse) -> m (Module (Malgo 'Rename), RnState)
rename builtinEnv (Module modName ds) = do
  opt <- view malgoOpt
  (ds', rnState) <- runStateT ?? RnState mempty modName $ runReaderT ?? RenameApp opt builtinEnv $ rnDecls ds
  pure (Module modName $ makeBindGroup ds', rnState)

resolveName :: MonadUniq m => String -> m RnId
resolveName name = newLocalId name ()

resolveGlobalName :: MonadUniq m => ModuleName -> String -> m RnId
resolveGlobalName modName name = newGlobalId name () modName

lookupVarName :: (MonadReader env m, HasRnEnv env, HasOpt env, MonadIO m) => SourcePos -> String -> m RnId
lookupVarName pos name = do
  view (rnEnv . varEnv . at name) >>= \case
    Just (name : _) -> pure name
    _ -> errorOn pos $ "Not in scope:" <+> quotes (text name)

lookupTypeName :: (MonadReader env m, HasRnEnv env, HasOpt env, MonadIO m) => SourcePos -> String -> m RnId
lookupTypeName pos name = do
  view (rnEnv . typeEnv . at name) >>= \case
    Just (name : _) -> pure name
    _ -> errorOn pos $ "Not in scope:" <+> quotes (text name)

lookupFieldName :: (MonadReader env m, HasRnEnv env, HasOpt env, MonadIO m) => SourcePos -> String -> m RnId
lookupFieldName pos name = do
  view (rnEnv . fieldEnv . at name) >>= \case
    Just (name : _) -> pure name
    _ -> errorOn pos $ "Not in scope:" <+> quotes (text name)

-- renamer

rnDecls ::
  (MonadUniq m, MonadReader env m, MonadState RnState m, HasRnEnv env, HasOpt env, MonadIO m) =>
  [Decl (Malgo 'Parse)] ->
  m [Decl (Malgo 'Rename)]
rnDecls ds = do
  modName <- use moduleName
  -- RnEnvの生成
  toplevelEnv <- genToplevelEnv modName ds
  -- RnStateの生成
  --   定義されていない識別子に対するInfixはエラー
  local (over rnEnv (toplevelEnv <>)) $ do
    rnState <- RnState <$> infixDecls ds <*> use moduleName
    -- 生成したRnEnv, RnStateの元でtraverse rnDecl ds
    put rnState
    traverse rnDecl ds

-- Declで定義されるトップレベル識別子はすでにRnEnvに正しく登録されているとする
-- infix宣言はすでに解釈されRnStateに登録されているとする
rnDecl ::
  (MonadUniq m, MonadReader env m, MonadState RnState m, HasOpt env, HasRnEnv env, MonadIO m) =>
  Decl (Malgo 'Parse) ->
  m (Decl (Malgo 'Rename))
rnDecl (ScDef pos name expr) = ScDef pos <$> lookupVarName pos name <*> rnExp expr
rnDecl (ScSig pos name typ) = do
  let tyVars = HashSet.toList $ getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (over rnEnv $ appendRnEnv typeEnv (zip tyVars tyVars')) $
    ScSig pos
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (DataDef pos name params cs) = do
  params' <- traverse resolveName params
  local (over rnEnv $ appendRnEnv typeEnv (zip params params')) $
    DataDef pos
      <$> lookupTypeName pos name
      <*> pure params'
      <*> traverse (bitraverse (lookupVarName pos) (traverse rnType)) cs
rnDecl (TypeSynonym pos name params typ) = do
  params' <- traverse resolveName params
  local (over rnEnv $ appendRnEnv typeEnv (zip params params')) $
    TypeSynonym pos <$> lookupTypeName pos name
      <*> pure params'
      <*> rnType typ
rnDecl (Infix pos assoc prec name) = Infix pos assoc prec <$> lookupVarName pos name
rnDecl (Foreign pos name typ) = do
  let tyVars = HashSet.toList $ getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (over rnEnv $ appendRnEnv typeEnv (zip tyVars tyVars')) $
    Foreign (pos, name)
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (Import pos modName) = do
  interface <- loadInterface modName
  infixInfo <>= interface ^. infixMap
  pure $ Import pos modName

-- 名前解決の他に，infix宣言に基づくOpAppの再構成も行う
rnExp ::
  (MonadReader env m, MonadState RnState m, MonadUniq m, MonadIO m, HasRnEnv env, HasOpt env) =>
  Exp (Malgo 'Parse) ->
  m (Exp (Malgo 'Rename))
rnExp (Var pos name) = Var pos <$> lookupVarName pos name
rnExp (Con pos name) = Con pos <$> lookupVarName pos name
rnExp (Unboxed pos val) = pure $ Unboxed pos val
rnExp (Boxed pos val) = do
  f <- lookupBox pos val
  pure $ Apply pos f (Unboxed pos $ toUnboxed val)
rnExp (Apply pos e1 e2) = Apply pos <$> rnExp e1 <*> rnExp e2
rnExp (OpApp pos op e1 e2) = do
  op' <- lookupVarName pos op
  e1' <- rnExp e1
  e2' <- rnExp e2
  mfixity <- HashMap.lookup op' <$> use infixInfo
  case mfixity of
    Just fixity -> mkOpApp pos fixity op' e1' e2'
    Nothing -> errorOn pos $ "No infix declaration:" <+> quotes (pPrint op)
rnExp (Fn pos cs) = Fn pos <$> traverse rnClause cs
rnExp (Tuple pos es) = Tuple pos <$> traverse rnExp es
rnExp (Record pos kvs) = Record pos <$> traverse (bitraverse (lookupFieldName pos) rnExp) kvs
rnExp (Force pos e) = Force pos <$> rnExp e
rnExp (Access pos l) = Access pos <$> lookupFieldName pos l
rnExp (Parens pos e) = Parens pos <$> rnExp e

lookupBox :: (MonadReader env f, MonadIO f, HasOpt env, HasRnEnv env) => SourcePos -> Literal x -> f (Exp (Malgo 'Rename))
lookupBox pos Int32 {} = Var pos <$> lookupVarName pos "int32#"
lookupBox pos Int64 {} = Var pos <$> lookupVarName pos "int64#"
lookupBox pos Float {} = Var pos <$> lookupVarName pos "float#"
lookupBox pos Double {} = Var pos <$> lookupVarName pos "double#"
lookupBox pos Char {} = Var pos <$> lookupVarName pos "char#"
lookupBox pos String {} = Var pos <$> lookupVarName pos "string#"

rnType :: (MonadReader env m, MonadIO m, HasOpt env, HasRnEnv env) => Type (Malgo 'Parse) -> m (Type (Malgo 'Rename))
rnType (TyApp pos t ts) = TyApp pos <$> rnType t <*> traverse rnType ts
rnType (TyVar pos x) = TyVar pos <$> lookupTypeName pos x
rnType (TyCon pos x) = TyCon pos <$> lookupTypeName pos x
rnType (TyArr pos t1 t2) = TyArr pos <$> rnType t1 <*> rnType t2
rnType (TyTuple pos ts) = TyTuple pos <$> traverse rnType ts
rnType (TyRecord pos kts) = TyRecord pos <$> traverse (bitraverse (lookupFieldName pos) rnType) kts
rnType (TyLazy pos t) = TyLazy pos <$> rnType t

rnClause ::
  (MonadUniq m, MonadReader env m, MonadState RnState m, MonadIO m, HasOpt env, HasRnEnv env) =>
  Clause (Malgo 'Parse) ->
  m (Clause (Malgo 'Rename))
rnClause (Clause pos ps ss) = do
  let vars = concatMap patVars ps
  -- varsに重複がないことを確認
  when (anySame vars) $ errorOn pos "Same variables occurs in a pattern"
  vm <- zip vars <$> traverse resolveName vars
  local (over rnEnv $ appendRnEnv varEnv vm) $ Clause pos <$> traverse rnPat ps <*> rnStmts ss
  where
    patVars (VarP _ x) = [x]
    patVars (ConP _ _ xs) = concatMap patVars xs
    patVars (TupleP _ xs) = concatMap patVars xs
    patVars (RecordP _ kvs) = concatMap (patVars . snd) kvs
    patVars UnboxedP {} = []

rnPat :: (MonadReader env m, MonadIO m, HasOpt env, HasRnEnv env) => Pat (Malgo 'Parse) -> m (Pat (Malgo 'Rename))
rnPat (VarP pos x) = VarP pos <$> lookupVarName pos x
rnPat (ConP pos x xs) = ConP pos <$> lookupVarName pos x <*> traverse rnPat xs
rnPat (TupleP pos xs) = TupleP pos <$> traverse rnPat xs
rnPat (RecordP pos kvs) = RecordP pos <$> traverse (bitraverse (lookupFieldName pos) rnPat) kvs
rnPat (UnboxedP pos x) = pure $ UnboxedP pos x

rnStmts :: (MonadReader env m, MonadState RnState m, MonadUniq m, HasOpt env, HasRnEnv env, MonadIO m) => [Stmt (Malgo 'Parse)] -> m [Stmt (Malgo 'Rename)]
rnStmts [] = pure []
rnStmts (NoBind x e : ss) = do
  e' <- rnExp e
  ss' <- rnStmts ss
  pure $ NoBind x e' : ss'
rnStmts (Let x v e : ss) = do
  e' <- rnExp e
  v' <- resolveName v
  local (over rnEnv $ appendRnEnv varEnv [(v, v')]) do
    ss' <- rnStmts ss
    pure $ Let x v' e' : ss'

-- infix宣言をMapに変換
infixDecls :: (MonadReader env m, HasRnEnv env, HasOpt env, MonadIO m) => [Decl (Malgo 'Parse)] -> m (HashMap RnId (Assoc, Int))
infixDecls ds =
  foldMapA ?? ds $ \case
    (Infix pos assoc order name) -> do
      name' <- lookupVarName pos name
      pure $ HashMap.singleton name' (assoc, order)
    _ -> pure mempty

mkOpApp ::
  (MonadIO m, MonadReader env m, HasOpt env) =>
  SourcePos ->
  (Assoc, Int) ->
  RnId ->
  Exp (Malgo 'Rename) ->
  Exp (Malgo 'Rename) ->
  m (Exp (Malgo 'Rename))
-- (e11 op1 e12) op2 e2
mkOpApp pos2 fix2 op2 (OpApp (pos1, fix1) op1 e11 e12) e2
  | nofix_error =
    errorOn pos1 $
      "Precedence parsing error:"
        $+$ nest
          2
          ( "cannot mix"
              <+> quotes (pPrint op1)
              <+> brackets (pPrint fix1)
              <+> "and"
              <+> quotes (pPrint op2)
              <+> brackets (pPrint fix2)
              <+> "in the same infix expression"
          )
  | associate_right = pure $ OpApp (pos1, fix1) op1 e11 (OpApp (pos2, fix2) op2 e12 e2)
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2
mkOpApp pos fix op e1 e2 = pure $ OpApp (pos, fix) op e1 e2

compareFixity :: (Assoc, Int) -> (Assoc, Int) -> (Bool, Bool)
compareFixity (assoc1, prec1) (assoc2, prec2) = case prec1 `compare` prec2 of
  GT -> left
  LT -> right
  EQ -> case (assoc1, assoc2) of
    (RightA, RightA) -> right
    (LeftA, LeftA) -> left
    _ -> error_please
  where
    right = (False, True)
    left = (False, False)
    error_please = (True, False)

genToplevelEnv :: (MonadUniq f, MonadReader env f, HasOpt env, MonadIO f) => ModuleName -> [Decl (Malgo 'Parse)] -> f RnEnv
genToplevelEnv modName ds = do
  execStateT (traverse aux ds) mempty
  where
    aux (ScDef pos x _) = do
      env <- use varEnv
      when (x `elem` HashMap.keys env) do
        errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      x' <- resolveGlobalName modName x
      modify $ appendRnEnv varEnv [(x, x')]
    aux ScSig {} = pure ()
    aux (DataDef pos x _ cs) = do
      env <- get
      when (x `elem` HashMap.keys (env ^. typeEnv)) do
        errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      unless (disjoint (map fst cs) (HashMap.keys (env ^. varEnv))) do
        errorOn pos $
          "Duplicate name(s):"
            <+> sep
              (punctuate "," $ map (quotes . pPrint) (map fst cs `intersect` HashMap.keys (env ^. varEnv)))
      x' <- resolveGlobalName modName x
      xs' <- traverse (resolveGlobalName modName . fst) cs
      modify $ appendRnEnv varEnv (zip (map fst cs) xs')
      modify $ appendRnEnv typeEnv [(x, x')]
      traverse_ (traverse_ genFieldEnv . snd) cs
    aux (TypeSynonym pos x _ t) = do
      env <- get
      when (x `elem` HashMap.keys (env ^. typeEnv)) do
        errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      x' <- resolveGlobalName modName x
      modify $ appendRnEnv typeEnv [(x, x')]
      genFieldEnv t
    aux (Foreign pos x _) = do
      env <- get
      when (x `elem` HashMap.keys (env ^. varEnv)) do
        errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      x' <- newGlobalId x () modName
      modify $ appendRnEnv varEnv [(x, x')]
    aux (Import _ modName') = do
      interface <- loadInterface modName'
      opt <- getOpt
      when (debugMode opt) $
        liftIO $ hPrint stderr $ pPrint interface
      modify $ appendRnEnv varEnv (HashMap.toList $ interface ^. resolvedVarIdentMap)
      modify $ appendRnEnv typeEnv (HashMap.toList $ interface ^. resolvedTypeIdentMap)
    aux Infix {} = pure ()
    genFieldEnv (TyApp _ t ts) = genFieldEnv t >> traverse_ genFieldEnv ts
    genFieldEnv (TyVar _ _) = pure ()
    genFieldEnv (TyCon _ _) = pure ()
    genFieldEnv (TyArr _ t1 t2) = genFieldEnv t1 >> genFieldEnv t2
    genFieldEnv (TyTuple _ ts) = traverse_ genFieldEnv ts
    genFieldEnv (TyRecord pos kts) = do
      let ks = map fst kts
      let ts = map snd kts
      traverse_ genFieldEnv ts
      env <- get
      unless (disjoint ks (HashMap.keys (env ^. fieldEnv))) do
        errorOn pos $
          "Duplicate name:"
            <+> sep
              (punctuate "," $ map (quotes . text) (ks `intersect` HashMap.keys (env ^. fieldEnv)))
      ks' <- traverse (resolveGlobalName modName) ks
      zipWithM_ (\k k' -> modify $ appendRnEnv fieldEnv [(k, k')]) ks ks'
    genFieldEnv (TyLazy _ t) = genFieldEnv t

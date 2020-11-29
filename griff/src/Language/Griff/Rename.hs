{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | 名前解決
module Language.Griff.Rename where

import Data.List (intersect)
import Data.List.Extra (disjoint)
import Data.List.Predicate (allUnique)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Griff.Extension
import Language.Griff.Prelude
import Language.Griff.RnEnv
import Language.Griff.Syntax
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.PrettyPrint as P
import qualified Data.Text.Lazy.IO as TL
import System.IO (stderr)
import Language.Griff.Interface

rename :: (MonadUniq m, MonadGriff m, MonadIO m) => RnEnv -> Module (Griff 'Parse) -> m [Decl (Griff 'Rename)]
rename rnEnv (Module modName ds) = do
  rnState <- genRnState modName
  evalStateT ?? rnState $ runReaderT ?? rnEnv $ rnDecls ds

resolveName :: (MonadUniq m, MonadState RnState m) => String -> m RnId
resolveName name = newId name =<< use moduleName

resolveGlobalName :: (MonadUniq m, MonadState RnState m) => String -> m RnId
resolveGlobalName name = newGlobalId name =<< use moduleName

lookupVarName :: (MonadReader RnEnv m, MonadGriff m, MonadIO m) => SourcePos -> String -> m RnId
lookupVarName pos name = do
  vm <- asks $ view varEnv
  case vm ^. at name of
    Just name' -> pure name'
    Nothing -> errorOn pos $ "Not in scope:" <+> P.quotes (pPrint name)

lookupTypeName :: (MonadReader RnEnv m, MonadGriff m, MonadIO m) => SourcePos -> String -> m RnId
lookupTypeName pos name = do
  tm <- asks $ view typeEnv
  case tm ^. at name of
    Just name' -> pure name'
    Nothing -> errorOn pos $ "Not in scope:" <+> P.quotes (pPrint name)

-- renamer

rnDecls ::
  (MonadUniq m, MonadReader RnEnv m, MonadState RnState m, MonadGriff m, MonadIO m) =>
  [Decl (Griff 'Parse)] ->
  m [Decl (Griff 'Rename)]
rnDecls ds = do
  -- RnEnvの生成
  (varNames, typeNames) <- toplevelIdents ds
  vm <- foldMapA (\v -> Map.singleton v <$> resolveGlobalName v) varNames
  tm <- foldMapA (\v -> Map.singleton v <$> resolveGlobalName v) typeNames
  let rnEnv = RnEnv vm tm
  -- RnStateの生成
  --   定義されていない識別子に対するInfixはエラー
  local (rnEnv <>) $ do
    rnState <- RnState <$> infixDecls ds <*> use moduleName
    -- 生成したRnEnv, RnStateの元でtraverse rnDecl ds
    put rnState
    traverse rnDecl ds

-- Declで定義されるトップレベル識別子はすでにRnEnvに正しく登録されているとする
-- infix宣言はすでに解釈されRnStateに登録されているとする
rnDecl ::
  (MonadUniq m, MonadReader RnEnv m, MonadState RnState m, MonadGriff m, MonadIO m) =>
  Decl (Griff 'Parse) ->
  m (Decl (Griff 'Rename))
rnDecl (ScDef pos name params expr) = do
  -- Desugarの都合上の制約
  -- Ref: Language.Griff.Desugar.curryFun
  case (params, expr) of
    ([], Fn {}) -> pure ()
    ([], _) -> errorOn pos "Toplevel definition must be `x = {...}` or `f x ... = ...`"
    _ -> pure ()
  params' <- traverse resolveName params
  local (over varEnv (Map.fromList (zip params params') <>)) $
    ScDef pos
      <$> lookupVarName pos name
      <*> pure params'
      <*> rnExp expr
rnDecl (ScSig pos name typ) = do
  let tyVars = Set.toList $ getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (over typeEnv (Map.fromList (zip tyVars tyVars') <>)) $
    ScSig pos
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (DataDef pos name params cs) = do
  params' <- traverse resolveName params
  local (over typeEnv (Map.fromList (zip params params') <>)) $
    DataDef pos
      <$> lookupTypeName pos name
      <*> pure params'
      <*> traverse (bitraverse (lookupVarName pos) (traverse rnType)) cs
rnDecl (Infix pos assoc prec name) = Infix pos assoc prec <$> lookupVarName pos name
rnDecl (Foreign pos name typ) = do
  let tyVars = Set.toList $ getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (over typeEnv (Map.fromList (zip tyVars tyVars') <>)) $
    Foreign (pos, name)
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (Import pos modName) = do
  -- identMap <- deserializeIdentMap modName
  -- opt <- getOpt
  -- when (debugMode opt) $ do
  --   liftIO $ TL.hPutStrLn stderr $ pShow identMap
  pure $ Import pos modName

-- 名前解決の他に，infix宣言に基づくOpAppの再構成も行う
rnExp ::
  (MonadReader RnEnv m, MonadState RnState m, MonadUniq m, MonadGriff m, MonadIO m) =>
  Exp (Griff 'Parse) ->
  m (Exp (Griff 'Rename))
rnExp (Var pos name) = Var pos <$> lookupVarName pos name
rnExp (Con pos name) = Con pos <$> lookupVarName pos name
rnExp (Unboxed pos val) = pure $ Unboxed pos val
rnExp (Apply pos e1 e2) = Apply pos <$> rnExp e1 <*> rnExp e2
rnExp (OpApp pos op e1 e2) = do
  op' <- lookupVarName pos op
  e1' <- rnExp e1
  e2' <- rnExp e2
  mfixity <- Map.lookup op' <$> use infixInfo
  case mfixity of
    Just fixity -> mkOpApp pos fixity op' e1' e2'
    Nothing -> errorOn pos $ "No infix declaration:" <+> P.quotes (pPrint op)
rnExp (Fn pos cs) = Fn pos <$> traverse rnClause cs
rnExp (Tuple pos es) = Tuple pos <$> traverse rnExp es
rnExp (Force pos e) = Force pos <$> rnExp e

rnType :: (MonadReader RnEnv m, MonadGriff m, MonadIO m) => Type (Griff 'Parse) -> m (Type (Griff 'Rename))
rnType (TyApp pos t ts) = TyApp pos <$> rnType t <*> traverse rnType ts
rnType (TyVar pos x) = TyVar pos <$> lookupTypeName pos x
rnType (TyCon pos x) = TyCon pos <$> lookupTypeName pos x
rnType (TyArr pos t1 t2) = TyArr pos <$> rnType t1 <*> rnType t2
rnType (TyTuple pos ts) = TyTuple pos <$> traverse rnType ts
rnType (TyLazy pos t) = TyLazy pos <$> rnType t

rnClause ::
  (MonadUniq m, MonadReader RnEnv m, MonadState RnState m, MonadGriff m, MonadIO m) =>
  Clause (Griff 'Parse) ->
  m (Clause (Griff 'Rename))
rnClause (Clause pos ps ss) = do
  let vars = concatMap patVars ps
  -- varsに重複がないことを確認
  unless (allUnique vars) $ errorOn pos "Same variables occurs in a pattern"
  vars' <- traverse resolveName vars
  let vm = Map.fromList $ zip vars vars'
  local (over varEnv (vm <>)) $ Clause pos <$> traverse rnPat ps <*> rnStmts ss
  where
    patVars (VarP _ x) = [x]
    patVars (ConP _ _ xs) = concatMap patVars xs
    patVars UnboxedP {} = []

rnPat :: (MonadReader RnEnv m, MonadGriff m, MonadIO m) => Pat (Griff 'Parse) -> m (Pat (Griff 'Rename))
rnPat (VarP pos x) = VarP pos <$> lookupVarName pos x
rnPat (ConP pos x xs) = ConP pos <$> lookupVarName pos x <*> traverse rnPat xs
rnPat (UnboxedP pos x) = pure $ UnboxedP pos x

rnStmts :: (MonadReader RnEnv m, MonadState RnState m, MonadUniq m, MonadGriff m, MonadIO m) => [Stmt (Griff 'Parse)] -> m [Stmt (Griff 'Rename)]
rnStmts [] = pure []
rnStmts (NoBind x e : ss) = do
  e' <- rnExp e
  ss' <- rnStmts ss
  pure $ NoBind x e' : ss'
rnStmts (Let x v e : ss) = do
  e' <- rnExp e
  v' <- resolveName v
  local (over varEnv (Map.insert v v')) do
    ss' <- rnStmts ss
    pure $ Let x v' e' : ss'

-- トップレベル識別子を列挙
toplevelIdents :: (MonadGriff m, MonadIO m) => [Decl (Griff 'Parse)] -> m ([String], [String])
toplevelIdents ds = do
  (sigs, vars, types) <- go ([], [], []) ds
  pure (ordNub $ sigs <> vars, types)
  where
    go result [] = pure result
    go (sigs, vars, types) (ScDef pos x _ _ : rest)
      | x `elem` vars = errorOn pos $ "Duplicate name:" <+> P.quotes (pPrint x)
      | otherwise = go (sigs, x : vars, types) rest
    go (sigs, vars, types) (ScSig pos x _ : rest)
      | x `elem` sigs = errorOn pos $ "Duplicate name:" <+> P.quotes (pPrint x)
      | otherwise = go (x : sigs, vars, types) rest
    go (sigs, vars, types) (DataDef pos x _ xs : rest)
      | x `elem` types = errorOn pos $ "Duplicate name:" <+> P.quotes (pPrint x)
      | disjoint (map fst xs) (sigs <> vars) = go (sigs, map fst xs <> vars, x : types) rest
      | otherwise =
        errorOn pos $
          "Duplicate name(s):"
            <+> P.sep
              (P.punctuate "," $ map (P.quotes . pPrint) (map fst xs `intersect` (sigs <> vars)))
    go (sigs, vars, types) (Foreign pos x _ : rest)
      | x `elem` sigs || x `elem` vars = errorOn pos $ "Duplicate name:" <+> P.quotes (pPrint x)
      | otherwise = go (sigs, x : vars, types) rest
    go (sigs, vars, types) (Import pos modName : rest) = do
      identMap <- deserializeIdentMap modName
      case identMap of
        Nothing -> errorOn pos $ "Module interface file is not found:" <+> P.quotes (pPrint modName)
        Just _ -> undefined
    go result (_ : rest) = go result rest

-- infix宣言をMapに変換
infixDecls :: (MonadReader RnEnv m, MonadGriff m, MonadIO m) => [Decl (Griff 'Parse)] -> m (Map RnId (Assoc, Int))
infixDecls ds = foldMapA ?? ds $ \case
  (Infix pos assoc order name) -> do
    name' <- lookupVarName pos name
    pure $ Map.singleton name' (assoc, order)
  _ -> pure mempty

mkOpApp ::
  (MonadGriff m, MonadIO m) =>
  SourcePos ->
  (Assoc, Int) ->
  RnId ->
  Exp (Griff 'Rename) ->
  Exp (Griff 'Rename) ->
  m (Exp (Griff 'Rename))
-- (e11 op1 e12) op2 e2
mkOpApp pos2 fix2 op2 (OpApp (pos1, fix1) op1 e11 e12) e2
  | nofix_error =
    errorOn pos1 $
      "Precedence parsing error:"
        P.$+$ P.nest
          2
          ( "cannot mix"
              <+> P.quotes (pPrint op1)
              <+> P.brackets (pPrint fix1)
              <+> "and"
              <+> P.quotes (pPrint op2)
              <+> P.brackets (pPrint fix2)
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

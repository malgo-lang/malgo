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

import Data.List (intersect)
import Data.List.Extra (disjoint)
import Data.List.Predicate (allUnique)
import qualified Data.Map as Map
import qualified Data.Set as Set
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

rename :: (MonadUniq m, MonadMalgo m, MonadIO m) => RnEnv -> Module (Malgo 'Parse) -> m (Module (Malgo 'Rename), RnState)
rename builtinEnv (Module modName ds) = do
  (ds', rnState) <- runStateT ?? RnState mempty modName $ runReaderT ?? builtinEnv $ rnDecls ds
  pure (Module modName $ makeBindGroup ds', rnState)

resolveName :: (MonadUniq m, MonadState RnState m) => ModuleName -> String -> m RnId
resolveName modName name = newLocalId name modName

resolveGlobalName :: (MonadUniq m, MonadState RnState m) => ModuleName -> String -> m RnId
resolveGlobalName modName name = newGlobalId name modName

resolveTopLevelName :: (MonadUniq m, MonadState RnState m) => ModuleName -> String -> m RnId
resolveTopLevelName modName name = newTopLevelId name modName

lookupQualifiedVarName :: (MonadReader RnEnv m, MonadMalgo m, MonadIO m) => SourcePos -> ModuleName -> String -> m (Id ModuleName)
lookupQualifiedVarName pos modName name = do
  vm <- view varEnv
  case find ((== modName) . view idMeta) =<< vm ^. at name of
    Just name -> pure name
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint modName <> "." <> text name)

lookupQualifiedTypeName :: (MonadReader RnEnv m, MonadMalgo m, MonadIO m) => SourcePos -> ModuleName -> String -> m (Id ModuleName)
lookupQualifiedTypeName pos modName name = do
  vm <- view typeEnv
  case find ((== modName) . view idMeta) =<< vm ^. at name of
    Just name -> pure name
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint modName <> "." <> text name)

lookupVarName :: (HasCallStack, MonadReader RnEnv m, MonadMalgo m, MonadIO m) => SourcePos -> String -> m RnId
lookupVarName pos name = do
  vm <- view varEnv
  case vm ^. at name of
    Just (name:_) -> pure name
    _ -> errorOn pos $ "Not in scope:" <+> quotes (text name)

lookupTypeName :: (HasCallStack, MonadReader RnEnv m, MonadMalgo m, MonadIO m) => SourcePos -> String -> m RnId
lookupTypeName pos name = do
  tm <- view typeEnv
  case tm ^. at name of
    Just (name:_) -> pure name
    _ -> errorOn pos $ "Not in scope:" <+> quotes (text name)

-- renamer

rnDecls ::
  (MonadUniq m, MonadReader RnEnv m, MonadState RnState m, MonadMalgo m, MonadIO m) =>
  [Decl (Malgo 'Parse)] ->
  m [Decl (Malgo 'Rename)]
rnDecls ds = do
  -- RnEnvの生成
  rnEnv <- genToplevelEnv ds
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
  (MonadUniq m, MonadReader RnEnv m, MonadState RnState m, MonadMalgo m, MonadIO m) =>
  Decl (Malgo 'Parse) ->
  m (Decl (Malgo 'Rename))
rnDecl (ScDef pos name expr) = ScDef pos <$> lookupVarName pos name <*> rnExp expr
rnDecl (ScSig pos name typ) = do
  let tyVars = Set.toList $ getTyVars typ
  modName <- use moduleName
  tyVars' <- traverse (resolveName modName) tyVars
  local (appendRnEnv typeEnv (zip tyVars tyVars')) $
    ScSig pos
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (DataDef pos name params cs) = do
  modName <- use moduleName
  params' <- traverse (resolveName modName) params
  local (appendRnEnv typeEnv (zip params params')) $
    DataDef pos
      <$> lookupTypeName pos name
      <*> pure params'
      <*> traverse (bitraverse (lookupVarName pos) (traverse rnType)) cs
rnDecl (Infix pos assoc prec name) = Infix pos assoc prec <$> lookupVarName pos name
rnDecl (Foreign pos name typ) = do
  let tyVars = Set.toList $ getTyVars typ
  modName <- use moduleName
  tyVars' <- traverse (resolveName modName) tyVars
  local (appendRnEnv typeEnv (zip tyVars tyVars')) $
    Foreign (pos, name)
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (Import pos modName) = do
  interface <- loadInterface modName
  infixInfo <>= interface ^. infixMap
  pure $ Import pos modName

-- 名前解決の他に，infix宣言に基づくOpAppの再構成も行う
rnExp ::
  (MonadReader RnEnv m, MonadState RnState m, MonadUniq m, MonadMalgo m, MonadIO m) =>
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
  mfixity <- Map.lookup op' <$> use infixInfo
  case mfixity of
    Just fixity -> mkOpApp pos fixity op' e1' e2'
    Nothing -> errorOn pos $ "No infix declaration:" <+> quotes (pPrint op)
rnExp (Fn pos cs) = Fn pos <$> traverse rnClause cs
rnExp (Tuple pos es) = Tuple pos <$> traverse rnExp es
rnExp (Force pos e) = Force pos <$> rnExp e
rnExp (Parens pos e) = Parens pos <$> rnExp e

lookupBox :: (MonadReader RnEnv f, MonadMalgo f, MonadIO f) => SourcePos -> Literal x -> f (Exp (Malgo 'Rename))
lookupBox pos Int32{} = Var pos <$> lookupVarName pos "int32#"
lookupBox pos Int64{} = Var pos <$> lookupVarName pos "int64#"
lookupBox pos Float{} = Var pos <$> lookupVarName pos "float#"
lookupBox pos Double{} = Var pos <$> lookupVarName pos "double#"
lookupBox pos Char{} = Var pos <$> lookupVarName pos "char#"
lookupBox pos String{} = Var pos <$> lookupVarName pos "string#"

rnType :: (MonadReader RnEnv m, MonadMalgo m, MonadIO m) => Type (Malgo 'Parse) -> m (Type (Malgo 'Rename))
rnType (TyApp pos t ts) = TyApp pos <$> rnType t <*> traverse rnType ts
rnType (TyVar pos x) = TyVar pos <$> lookupTypeName pos x
rnType (TyCon pos x) = TyCon pos <$> lookupTypeName pos x
rnType (TyArr pos t1 t2) = TyArr pos <$> rnType t1 <*> rnType t2
rnType (TyTuple pos ts) = TyTuple pos <$> traverse rnType ts
rnType (TyLazy pos t) = TyLazy pos <$> rnType t

rnClause ::
  (MonadUniq m, MonadReader RnEnv m, MonadState RnState m, MonadMalgo m, MonadIO m) =>
  Clause (Malgo 'Parse) ->
  m (Clause (Malgo 'Rename))
rnClause (Clause pos ps ss) = do
  let vars = concatMap patVars ps
  -- varsに重複がないことを確認
  unless (allUnique vars) $ errorOn pos "Same variables occurs in a pattern"
  modName <- use moduleName
  vm <- zip vars <$> traverse (resolveName modName) vars
  local (appendRnEnv varEnv vm) $ Clause pos <$> traverse rnPat ps <*> rnStmts ss
  where
    patVars (VarP _ x) = [x]
    patVars (ConP _ _ xs) = concatMap patVars xs
    patVars (TupleP _ xs) = concatMap patVars xs
    patVars UnboxedP {} = []

rnPat :: (MonadReader RnEnv m, MonadMalgo m, MonadIO m) => Pat (Malgo 'Parse) -> m (Pat (Malgo 'Rename))
rnPat (VarP pos x) = VarP pos <$> lookupVarName pos x
rnPat (ConP pos x xs) = ConP pos <$> lookupVarName pos x <*> traverse rnPat xs
rnPat (TupleP pos xs) = TupleP pos <$> traverse rnPat xs
rnPat (UnboxedP pos x) = pure $ UnboxedP pos x

rnStmts :: (MonadReader RnEnv m, MonadState RnState m, MonadUniq m, MonadMalgo m, MonadIO m) => [Stmt (Malgo 'Parse)] -> m [Stmt (Malgo 'Rename)]
rnStmts [] = pure []
rnStmts (NoBind x e : ss) = do
  e' <- rnExp e
  ss' <- rnStmts ss
  pure $ NoBind x e' : ss'
rnStmts (Let x v e : ss) = do
  e' <- rnExp e
  modName <- use moduleName
  v' <- resolveName modName v
  local (appendRnEnv varEnv [(v, v')]) do
    ss' <- rnStmts ss
    pure $ Let x v' e' : ss'

genToplevelEnv :: (MonadMalgo m, MonadIO m, MonadUniq m, MonadState RnState m) => [Decl (Malgo 'Parse)] -> m RnEnv
genToplevelEnv ds = do
  modName <- use moduleName
  go modName mempty ds
  where
    go _ env [] = pure env
    go modName env (ScDef pos x _ : rest)
      | x `elem` Map.keys (env ^. varEnv) = errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      | otherwise = do
        x' <- resolveGlobalName modName x
        go modName (appendRnEnv varEnv [(x, x')] env) rest
    go modName env (ScSig {} : rest) = go modName env rest
    go modName env (DataDef pos x _ cs : rest)
      | x `elem` Map.keys (env ^. typeEnv) = errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      | disjoint (map fst cs) (Map.keys (env ^. varEnv)) = do
        x' <- resolveGlobalName modName x
        xs' <- traverse (resolveGlobalName modName . fst) cs
        -- go modName (env & varEnv <>~ Map.fromList (zip (map fst cs) xs') & typeEnv . at x ?~ x') rest
        go modName (appendRnEnv varEnv (zip (map fst cs) xs') $ appendRnEnv typeEnv [(x, x')] env) rest
      | otherwise =
        errorOn pos $
          "Duplicate name(s):"
            <+> sep
              (punctuate "," $ map (quotes . pPrint) (map fst cs `intersect` Map.keys (env ^. varEnv)))
    go modName env (Foreign pos x _ : rest)
      | x `elem` Map.keys (env ^. varEnv) = errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      | otherwise = do
        x' <- resolveTopLevelName modName x
        go modName (appendRnEnv varEnv [(x, x')] env) rest
    go modName env (Import _ modName' : rest) = do
      interface <- loadInterface modName'
      opt <- getOpt
      when (debugMode opt) $
        liftIO $ hPrint stderr $ pPrint interface
      go
        modName
        ( appendRnEnv varEnv (Map.toList $ interface ^. resolvedVarIdentMap) $
            appendRnEnv typeEnv (Map.toList $ interface ^. resolvedTypeIdentMap) env
        )
        rest
    go modName env (Infix {} : rest) = go modName env rest

-- infix宣言をMapに変換
infixDecls :: (MonadReader RnEnv m, MonadMalgo m, MonadIO m) => [Decl (Malgo 'Parse)] -> m (Map RnId (Assoc, Int))
infixDecls ds = foldMapA ?? ds $ \case
  (Infix pos assoc order name) -> do
    name' <- lookupVarName pos name
    pure $ Map.singleton name' (assoc, order)
  _ -> pure mempty

mkOpApp ::
  (MonadMalgo m, MonadIO m) =>
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

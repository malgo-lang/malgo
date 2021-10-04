-- | 名前解決
module Malgo.Rename.Pass where

import Control.Lens (At (at), over, use, view, (<>=), (^.), _2)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (intersect)
import Data.List.Extra (anySame, disjoint)
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Interface hiding (dependencies)
import Malgo.Prelude
import Malgo.Rename.RnEnv
import Malgo.Syntax
import Malgo.Syntax.Extension
import System.IO (hPrint)
import Text.Megaparsec.Pos (SourcePos)

rename :: MonadIO m => RnEnv -> Module (Malgo 'Parse) -> m (Module (Malgo 'Rename), RnState)
rename builtinEnv (Module modName (ParsedDefinitions ds)) = do
  (ds', rnState) <- runStateT ?? RnState mempty [] modName $ runReaderT ?? builtinEnv $ rnDecls ds
  pure (Module modName $ makeBindGroup ds', rnState)

resolveName :: (MonadReader env m, MonadIO m, HasUniqSupply env) => Text -> m RnId
resolveName name = newInternalId name ()

resolveGlobalName :: (MonadReader env m, MonadIO m, HasUniqSupply env) => ModuleName -> Text -> m RnId
resolveGlobalName modName name = newExternalId name () modName

lookupVarName :: (MonadReader RnEnv m, MonadIO m) => SourcePos -> Text -> m RnId
lookupVarName pos name =
  view (varEnv . at name) >>= \case
    Just names -> case find (\i -> i ^. ann == Implicit) names of
      Just (Annotated _ name) -> pure name
      Nothing ->
        errorOn pos $
          "Not in scope:" <+> quotes (pPrint name)
            $$ "Did you mean" <+> pPrint (map (view value) names)
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

lookupTypeName :: (MonadReader RnEnv m, MonadIO m) => SourcePos -> Text -> m RnId
lookupTypeName pos name =
  view (typeEnv . at name) >>= \case
    Just names -> case find (\i -> i ^. ann == Implicit) names of
      Just (Annotated _ name) -> pure name
      Nothing ->
        errorOn pos $
          "Not in scope:" <+> quotes (pPrint name)
            $$ "Did you mean" <+> pPrint (map (view value) names)
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

lookupFieldName :: (MonadReader RnEnv m, MonadIO m) => SourcePos -> Text -> m RnId
lookupFieldName pos name =
  view (fieldEnv . at name) >>= \case
    Just names -> case find (\i -> i ^. ann == Implicit) names of
      Just (Annotated _ name) -> pure name
      Nothing ->
        errorOn pos $
          "Not in scope:" <+> quotes (pPrint name)
            $$ "Did you mean" <+> pPrint (map (view value) names)
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

lookupQualifiedVarName :: (MonadReader RnEnv m, MonadIO m) => SourcePos -> ModuleName -> Text -> m (Id ())
lookupQualifiedVarName pos modName name =
  view (varEnv . at name) >>= \case
    Just names ->
      case find (\i -> i ^. ann == Explicit modName) names of
        Just (Annotated _ name) -> pure name
        Nothing ->
          errorOn pos $
            "Not in scope:" <+> quotes (pPrint name) <+> "in" <+> pPrint modName
              $$ "Did you mean" <+> "`" <> pPrint modName <+> "." <+> pPrint name <> "`" <+> "?"
    _ -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)

-- renamer

rnDecls ::
  (MonadReader RnEnv m, MonadState RnState m, MonadIO m) =>
  [Decl (Malgo 'Parse)] ->
  m [Decl (Malgo 'Rename)]
rnDecls ds = do
  modName <- use moduleName
  -- RnEnvの生成
  rnEnv <- genToplevelEnv modName ds =<< ask
  local (const rnEnv) $ do
    -- RnStateの生成
    put =<< RnState <$> infixDecls ds <*> pure [] <*> use moduleName
    -- 生成したRnEnv, RnStateの元でtraverse rnDecl ds
    traverse rnDecl ds

-- Declで定義されるトップレベル識別子はすでにRnEnvに正しく登録されているとする
-- infix宣言はすでに解釈されRnStateに登録されているとする
rnDecl ::
  (MonadReader RnEnv m, MonadState RnState m, MonadIO m) =>
  Decl (Malgo 'Parse) ->
  m (Decl (Malgo 'Rename))
rnDecl (ScDef pos name expr) = ScDef pos <$> lookupVarName pos name <*> rnExp expr
rnDecl (ScSig pos name typ) = do
  let tyVars = HashSet.toList $ getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (appendRnEnv typeEnv (zip tyVars $ map (Annotated Implicit) tyVars')) $
    ScSig pos
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (DataDef pos name params cs) = do
  params' <- traverse resolveName params
  local (appendRnEnv typeEnv (zip params $ map (Annotated Implicit) params')) $
    DataDef pos
      <$> lookupTypeName pos name
      <*> pure params'
      <*> traverse (bitraverse (lookupVarName pos) (traverse rnType)) cs
rnDecl (TypeSynonym pos name params typ) = do
  params' <- traverse resolveName params
  local (appendRnEnv typeEnv (zip params $ map (Annotated Implicit) params')) $
    TypeSynonym pos <$> lookupTypeName pos name
      <*> pure params'
      <*> rnType typ
rnDecl (Infix pos assoc prec name) = Infix pos assoc prec <$> lookupVarName pos name
rnDecl (Foreign pos name typ) = do
  let tyVars = HashSet.toList $ getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (appendRnEnv typeEnv (zip tyVars $ map (Annotated Implicit) tyVars')) $
    Foreign (pos, name)
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (Import pos modName importList) = do
  interface <-
    loadInterface modName >>= \case
      Just x -> pure x
      Nothing -> errorOn pos $ "module" <+> pPrint modName <+> "is not found"
  infixInfo <>= interface ^. infixMap
  dependencies <>= [modName]
  pure $ Import pos modName importList

-- 名前解決の他に，infix宣言に基づくOpAppの再構成も行う
rnExp ::
  (MonadReader RnEnv m, MonadState RnState m, MonadIO m) =>
  Exp (Malgo 'Parse) ->
  m (Exp (Malgo 'Rename))
rnExp (Var pos (WithPrefix (Annotated Nothing name))) = Var pos . NoPrefix <$> lookupVarName pos name
rnExp (Var pos (WithPrefix (Annotated (Just modName) name))) = Var pos . Prefix modName <$> lookupQualifiedVarName pos (ModuleName modName) name
rnExp (Unboxed pos val) = pure $ Unboxed pos val
rnExp (Boxed pos val) = do
  f <- lookupBox pos val
  pure $ Apply pos (Var pos (NoPrefix f)) (Unboxed pos $ toUnboxed val)
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
rnExp (Record pos kvs) =
  Record pos
    <$> traverse
      ( bitraverse
          (\(WithPrefix (Annotated p x)) -> WithPrefix . Annotated p <$> lookupFieldName pos x)
          rnExp
      )
      kvs
rnExp (List pos es) = do
  nilName <- lookupVarName pos "Nil"
  consName <- lookupVarName pos "Cons"
  buildListApply nilName consName <$> traverse rnExp es
  where
    buildListApply nilName _ [] = Var pos (NoPrefix nilName)
    buildListApply nilName consName (x : xs) = Apply pos (Apply pos (Var pos (NoPrefix consName)) x) (buildListApply nilName consName xs)
rnExp (RecordAccess pos (WithPrefix (Annotated p l))) = RecordAccess pos . WithPrefix . Annotated p <$> lookupFieldName pos l
rnExp (Ann pos e t) = Ann pos <$> rnExp e <*> rnType t
rnExp (Seq pos ss) = Seq pos <$> rnStmts ss
rnExp (Parens pos e) = Parens pos <$> rnExp e

lookupBox :: (MonadReader RnEnv f, MonadIO f) => SourcePos -> Literal x -> f (XId (Malgo 'Rename))
lookupBox pos Int32 {} = lookupVarName pos "Int32#"
lookupBox pos Int64 {} = lookupVarName pos "Int64#"
lookupBox pos Float {} = lookupVarName pos "Float#"
lookupBox pos Double {} = lookupVarName pos "Double#"
lookupBox pos Char {} = lookupVarName pos "Char#"
lookupBox pos String {} = lookupVarName pos "String#"

rnType :: (MonadReader RnEnv m, MonadIO m) => Type (Malgo 'Parse) -> m (Type (Malgo 'Rename))
rnType (TyApp pos t ts) = TyApp pos <$> rnType t <*> traverse rnType ts
rnType (TyVar pos x) = TyVar pos <$> lookupTypeName pos x
rnType (TyCon pos x) = TyCon pos <$> lookupTypeName pos x
rnType (TyArr pos t1 t2) = TyArr pos <$> rnType t1 <*> rnType t2
rnType (TyTuple pos ts) = TyTuple pos <$> traverse rnType ts
rnType (TyRecord pos kts) = TyRecord pos <$> traverse (bitraverse (lookupFieldName pos) rnType) kts
rnType (TyBlock pos t) = TyArr pos (TyTuple pos []) <$> rnType t
rnType (TyDArr pos _ _) = errorOn pos "not implemented"

rnClause ::
  (MonadReader RnEnv m, MonadState RnState m, MonadIO m) =>
  Clause (Malgo 'Parse) ->
  m (Clause (Malgo 'Rename))
rnClause (Clause pos ps e) = do
  let vars = concatMap patVars ps
  -- varsに重複がないことを確認
  when (anySame $ filter (/= "_") vars) $ errorOn pos "Same variables occurs in a pattern"
  vm <- zip vars . map (Annotated Implicit) <$> traverse resolveName vars
  local (appendRnEnv varEnv vm) $ Clause pos <$> traverse rnPat ps <*> rnExp e
  where
    patVars (VarP _ x) = [x]
    patVars (ConP _ _ xs) = concatMap patVars xs
    patVars (TupleP _ xs) = concatMap patVars xs
    patVars (RecordP _ kvs) = concatMap (patVars . snd) kvs
    patVars (ListP _ xs) = concatMap patVars xs
    patVars UnboxedP {} = []
    patVars BoxedP {} = []

rnPat :: (MonadReader RnEnv m, MonadIO m) => Pat (Malgo 'Parse) -> m (Pat (Malgo 'Rename))
rnPat (VarP pos x) = VarP pos <$> lookupVarName pos x
rnPat (ConP pos x xs) = ConP pos <$> lookupVarName pos x <*> traverse rnPat xs
rnPat (TupleP pos xs) = TupleP pos <$> traverse rnPat xs
rnPat (RecordP pos kvs) = RecordP pos <$> traverse (bitraverse (\(WithPrefix (Annotated p k)) -> WithPrefix . Annotated p <$> lookupFieldName pos k) rnPat) kvs
rnPat (ListP pos xs) = buildListP <$> lookupVarName pos "Nil" <*> lookupVarName pos "Cons" <*> traverse rnPat xs
  where
    buildListP nilName _ [] = ConP pos nilName []
    buildListP nilName consName (x : xs) = ConP pos consName [x, buildListP nilName consName xs]
rnPat (UnboxedP pos (String _)) = errorOn pos "String literal pattern is not supported"
rnPat (BoxedP pos (String _)) = errorOn pos "String literal pattern is not supported"
rnPat (UnboxedP pos x) = pure $ UnboxedP pos x
rnPat (BoxedP pos x) = ConP pos <$> lookupBox pos x <*> pure [UnboxedP pos (coerce x)]

rnStmts :: (MonadReader RnEnv m, MonadState RnState m, MonadIO m) => NonEmpty (Stmt (Malgo 'Parse)) -> m (NonEmpty (Stmt (Malgo 'Rename)))
rnStmts (NoBind x e :| []) = do
  e' <- rnExp e
  pure $ NoBind x e' :| []
rnStmts (Let x v e :| []) = do
  e' <- rnExp e
  v' <- resolveName v
  pure $ Let x v' e' :| []
rnStmts (NoBind x e :| s : ss) = do
  e' <- rnExp e
  s' :| ss' <- rnStmts (s :| ss)
  pure $ NoBind x e' :| s' : ss'
rnStmts (Let x v e :| s : ss) = do
  e' <- rnExp e
  v' <- resolveName v
  local (appendRnEnv varEnv [(v, Annotated Implicit v')]) do
    s' :| ss' <- rnStmts (s :| ss)
    pure $ Let x v' e' :| s' : ss'
rnStmts (With x (Just v) e :| s : ss) = do
  e <- rnExp e
  ss <- rnExp (Fn x $ Clause x [VarP x v] (Seq x $ s :| ss) :| [])
  pure $ NoBind x (Apply x e ss) :| []
rnStmts (With x Nothing e :| s : ss) = do
  e <- rnExp e
  ss <- rnExp (Fn x $ Clause x [] (Seq x $ s :| ss) :| [])
  pure $ NoBind x (Apply x e ss) :| []
rnStmts (With x _ _ :| []) = errorOn x "`with` statement cannnot appear in the last line of the sequence expression."

-- infix宣言をMapに変換
-- 定義されていない識別子に対するInfixはエラーとする
infixDecls :: (MonadReader RnEnv m, MonadIO m) => [Decl (Malgo 'Parse)] -> m (HashMap RnId (Assoc, Int))
infixDecls ds =
  foldMapM ?? ds $ \case
    (Infix pos assoc order name) -> do
      name' <- lookupVarName pos name
      pure $ one (name', (assoc, order))
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
  | associate_right = do
    e' <- mkOpApp pos2 fix2 op2 e12 e2
    pure $ OpApp (pos1, fix1) op1 e11 e'
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

genToplevelEnv :: (MonadReader env f, HasOpt env, MonadIO f, HasUniqSupply env) => ModuleName -> [Decl (Malgo 'Parse)] -> RnEnv -> f RnEnv
genToplevelEnv modName ds =
  execStateT (traverse aux ds)
  where
    aux (ScDef pos x _) = do
      env <- use varEnv
      when (x `elem` HashMap.keys env) do
        errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      x' <- resolveGlobalName modName x
      modify $ appendRnEnv varEnv [(x, Annotated Implicit x')]
    aux ScSig {} = pass
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
      modify $ appendRnEnv varEnv (zip (map fst cs) $ map (Annotated Implicit) xs')
      modify $ appendRnEnv typeEnv [(x, Annotated Implicit x')]
      traverse_ (traverse_ genFieldEnv . snd) cs
    aux (TypeSynonym pos x _ t) = do
      env <- get
      when (x `elem` HashMap.keys (env ^. typeEnv)) do
        errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      x' <- resolveGlobalName modName x
      modify $ appendRnEnv typeEnv [(x, Annotated Implicit x')]
      genFieldEnv t
    aux (Foreign pos x _) = do
      env <- get
      when (x `elem` HashMap.keys (env ^. varEnv)) do
        errorOn pos $ "Duplicate name:" <+> quotes (pPrint x)
      x' <- newExternalId x () modName
      modify $ appendRnEnv varEnv [(x, Annotated Implicit x')]
    aux (Import pos modName' All) = do
      interface <-
        loadInterface modName' >>= \case
          Just x -> pure x
          Nothing -> errorOn pos $ "module" <+> pPrint modName' <+> "is not found"
      opt <- getOpt
      when (debugMode opt) $
        liftIO $ hPrint stderr $ pPrint interface
      -- 全ての識別子をImplicitでimportする
      modify $ appendRnEnv varEnv (map (over _2 $ Annotated Implicit) $ HashMap.toList $ interface ^. resolvedVarIdentMap)
      modify $ appendRnEnv typeEnv (map (over _2 $ Annotated Implicit) $ HashMap.toList $ interface ^. resolvedTypeIdentMap)
    aux (Import pos modName' (Selected implicits)) = do
      interface <-
        loadInterface modName' >>= \case
          Just x -> pure x
          Nothing -> errorOn pos $ "module" <+> pPrint modName' <+> "is not found"
      opt <- getOpt
      when (debugMode opt) $
        liftIO $ hPrint stderr $ pPrint interface
      modify $
        appendRnEnv
          varEnv
          ( map
              ( \(name, id) ->
                  if name `elem` implicits
                    then (name, Annotated Implicit id)
                    else (name, Annotated (Explicit modName') id)
              )
              $ HashMap.toList $ interface ^. resolvedVarIdentMap
          )
      modify $
        appendRnEnv
          typeEnv
          ( map
              ( \(name, id) ->
                  if name `elem` implicits
                    then (name, Annotated Implicit id)
                    else (name, Annotated (Explicit modName') id)
              )
              $ HashMap.toList $ interface ^. resolvedTypeIdentMap
          )
    aux (Import pos modName' (As modNameAs)) = do
      interface <-
        loadInterface modName' >>= \case
          Just x -> pure x
          Nothing -> errorOn pos $ "module" <+> pPrint modName' <+> "is not found"
      opt <- getOpt
      when (debugMode opt) $
        liftIO $ hPrint stderr $ pPrint interface
      modify $ appendRnEnv varEnv (map (over _2 $ Annotated (Explicit modNameAs)) $ HashMap.toList $ interface ^. resolvedVarIdentMap)
      modify $ appendRnEnv typeEnv (map (over _2 $ Annotated (Explicit modNameAs)) $ HashMap.toList $ interface ^. resolvedTypeIdentMap)
    aux Infix {} = pass
    genFieldEnv (TyApp _ t ts) = genFieldEnv t >> traverse_ genFieldEnv ts
    genFieldEnv (TyVar _ _) = pass
    genFieldEnv (TyCon _ _) = pass
    genFieldEnv (TyArr _ t1 t2) = genFieldEnv t1 >> genFieldEnv t2
    genFieldEnv (TyTuple _ ts) = traverse_ genFieldEnv ts
    genFieldEnv (TyRecord _ kts) = do
      let ks = map fst kts
      let ts = map snd kts
      traverse_ genFieldEnv ts
      ks' <- traverse (resolveGlobalName modName) ks
      zipWithM_ (\k k' -> modify $ appendRnEnv fieldEnv [(k, Annotated Implicit k')]) ks ks'
    genFieldEnv (TyBlock _ t) = genFieldEnv t
    genFieldEnv (TyDArr _ t1 t2) = genFieldEnv t1 >> genFieldEnv t2

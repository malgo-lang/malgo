-- | Name resolution and simple desugar transformation
module Malgo.Rename.Pass (rename, RenamePass (..)) where

import Control.Lens (view, _2)
import Data.List (intersect)
import Data.List.Extra (anySame, disjoint)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static
import Effectful.Reader.Static (Reader, ask, asks, local, runReader)
import Effectful.State.Static.Local (State, execState, get, gets, modify, put, runState)
import Malgo.Id
import Malgo.Interface
import Malgo.Module
import Malgo.Pass (Pass (..))
import Malgo.Prelude hiding (All, catchError, throwError)
import Malgo.Rename.RnEnv
import Malgo.Rename.RnState as RnState
import Malgo.Syntax hiding (getTyVars)
import Malgo.Syntax.Extension
import Prettyprinter (brackets, nest, squotes, vsep, (<+>))

data RenamePass = RenamePass

instance Pass RenamePass where
  type Input RenamePass = (Module (Malgo NewParse), RnEnv)
  type Output RenamePass = (Module (Malgo Rename), RnState)
  type ErrorType RenamePass = RenameError
  type
    Effects RenamePass es =
      ( State (Map ModuleName Interface) :> es,
        State Uniq :> es,
        IOE :> es,
        Reader Flag :> es,
        Workspace :> es
      )

  runPassImpl _ (Module modName (ParsedDefinitions ds), builtinEnv) = do
    (ds', rnState) <- runState (RnState mempty Set.empty) $ runReader builtinEnv $ runReader modName $ rnDecls ds
    pure (Module modName $ makeBindGroup ds', rnState)

-- | Entry point of this 'Malgo.Rename.Pass'
rename ::
  (State (Map ModuleName Interface) :> es, State Uniq :> es, IOE :> es, Reader Flag :> es, Workspace :> es) =>
  RnEnv ->
  Module (Malgo NewParse) ->
  Eff es (Either (CallStack, RenameError) (Module (Malgo Rename), RnState))
rename builtinEnv (Module modName (ParsedDefinitions ds)) = runError do
  (ds', rnState) <- runState (RnState mempty Set.empty) $ runReader builtinEnv $ runReader modName $ rnDecls ds
  pure (Module modName $ makeBindGroup ds', rnState)

-- renamer

-- | Rename toplevel declarations
rnDecls ::
  ( Reader ModuleName :> es,
    Reader RnEnv :> es,
    State RnState :> es,
    State (Map ModuleName Interface) :> es,
    State Uniq :> es,
    IOE :> es,
    Reader Flag :> es,
    Workspace :> es,
    Error RenameError :> es
  ) =>
  [Decl (Malgo NewParse)] ->
  Eff es [Decl (Malgo Rename)]
rnDecls ds = do
  -- RnEnvの生成
  rnEnv <- genToplevelEnv ds =<< ask
  local (const rnEnv) $ do
    -- RnStateの生成
    put =<< RnState <$> infixDecls ds <*> pure Set.empty
    -- 生成したRnEnv, RnStateの元でtraverse rnDecl ds
    traverse rnDecl ds

-- | Rename a toplevel declaration.
-- It is assumed that the top-level identifier defined in Decl has already been correctly registered in RnEnv.
-- The infix declaration is assumed to have already been interpreted and registered in RnState.
rnDecl ::
  ( State RnState :> es,
    State (Map ModuleName Interface) :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es,
    Reader Flag :> es,
    Workspace :> es,
    Error RenameError :> es
  ) =>
  Decl (Malgo NewParse) ->
  Eff es (Decl (Malgo Rename))
rnDecl (ScDef pos name expr) = ScDef pos <$> lookupVarName pos name <*> rnExpr expr
rnDecl (ScSig pos name typ) = do
  tyVars <- Set.toList <$> getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (insertTypeIdent (zip tyVars $ map (Qualified Implicit) tyVars'))
    $ ScSig pos
    <$> lookupVarName pos name
    <*> rnType typ
rnDecl (DataDef pos name params cs) = do
  params' <- traverse (resolveName . snd) params
  local (insertTypeIdent (zip (map snd params) (map (Qualified Implicit) params')))
    $ DataDef pos
    <$> lookupTypeName pos name
    <*> pure (zipWith (\(range, _) p' -> (range, p')) params params')
    <*> traverse (bitraverse (lookupVarName pos) (traverse rnType)) cs
rnDecl (TypeSynonym pos name params typ) = do
  params' <- traverse resolveName params
  local (insertTypeIdent (zip params $ map (Qualified Implicit) params'))
    $ TypeSynonym pos
    <$> lookupTypeName pos name
    <*> pure params'
    <*> rnType typ
rnDecl (Infix pos assoc prec name) = Infix pos assoc prec <$> lookupVarName pos name
rnDecl (Foreign pos name typ) = do
  tyVars <- Set.toList <$> getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (insertTypeIdent (zip tyVars $ map (Qualified Implicit) tyVars'))
    $ Foreign (pos, name)
    <$> lookupVarName pos name
    <*> rnType typ
rnDecl (Import pos modName importList) = do
  interface <- loadInterface modName
  modify \s@RnState {..} ->
    s
      { RnState.infixInfo = s.infixInfo <> Map.mapKeys (externalFromInterface interface) interface.infixInfo,
        RnState.dependencies = Set.insert modName dependencies <> interface.dependencies
      }
  pure $ Import pos modName importList

-- | Get type variables from a type.
getTyVars :: Type (Malgo NewParse) -> Eff es (Set Text)
getTyVars (TyApp _ t ts) = do
  t <- getTyVars t
  ts <- mconcat <$> traverse getTyVars ts
  pure $ t <> ts
getTyVars (TyVar _ v)
  | isUpper (T.head v) = pure mempty
  | otherwise = pure $ Set.singleton v
getTyVars (TyArr _ t1 t2) = getTyVars t1 <> getTyVars t2
getTyVars (TyTuple _ ts) = mconcat $ map getTyVars ts
getTyVars (TyRecord _ kvs) = mconcat $ map (getTyVars . snd) kvs
getTyVars (TyBlock _ t) = getTyVars t

-- | Rename a expression.
-- In addition to name resolution, OpApp recombination based on infix declarations is also performed.
rnExpr ::
  ( State RnState :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es,
    Reader Flag :> es,
    Error RenameError :> es
  ) =>
  Expr (Malgo NewParse) ->
  Eff es (Expr (Malgo Rename))
rnExpr (Var pos name) = Var pos <$> lookupVarName pos name
rnExpr (Unboxed pos val) = pure $ Unboxed pos val
rnExpr (Boxed pos val) = do
  f <- lookupBox pos val
  pure $ Apply pos (Var pos f) (Unboxed pos $ toUnboxed val)
rnExpr (Apply pos e1 e2) = Apply pos <$> rnExpr e1 <*> rnExpr e2
rnExpr (OpApp pos op e1 e2) = do
  op' <- lookupVarName pos op
  e1' <- rnExpr e1
  e2' <- rnExpr e2
  mfixity <- Map.lookup op' <$> gets @RnState (.infixInfo)
  case mfixity of
    Just fixity -> mkOpApp pos fixity op' e1' e2'
    Nothing -> errorOn pos $ "No infix declaration:" <+> squotes (pretty op)
rnExpr (Project pos (Var _ name) field) = do
  moduleNames <- asks @RnEnv (.moduleNames)
  if ModuleName name `Set.member` moduleNames
    then Var pos <$> lookupQualifiedVarName pos (ModuleName name) field
    else Project pos <$> rnExpr (Var pos name) <*> pure field
rnExpr (Project pos expr field) = Project pos <$> rnExpr expr <*> pure field
rnExpr (Fn pos cs) = Fn pos <$> traverse rnClause cs
rnExpr (Tuple pos es) = Tuple pos <$> traverse rnExpr es
rnExpr (Record pos kvs) =
  Record pos
    <$> traverse
      ( bitraverse
          pure
          rnExpr
      )
      kvs
rnExpr (List pos es) = do
  nilName <- lookupVarName pos "Nil"
  consName <- lookupVarName pos "Cons"
  buildListApply nilName consName <$> traverse rnExpr es
  where
    buildListApply nilName _ [] = Var pos nilName
    buildListApply nilName consName (x : xs) = Apply pos (Apply pos (Var pos consName) x) (buildListApply nilName consName xs)
rnExpr (Ann pos e t) = Ann pos <$> rnExpr e <*> rnType t
rnExpr (Seq pos ss) = Seq pos <$> rnStmts ss
rnExpr (Parens _ e) = rnExpr e

-- | Renamed identifier corresponding Boxed literals.
lookupBox :: (Reader RnEnv :> es, Error RenameError :> es) => Range -> Literal x -> Eff es Id
lookupBox pos Int32 {} = lookupVarName pos "Int32#"
lookupBox pos Int64 {} = lookupVarName pos "Int64#"
lookupBox pos Float {} = lookupVarName pos "Float#"
lookupBox pos Double {} = lookupVarName pos "Double#"
lookupBox pos Char {} = lookupVarName pos "Char#"
lookupBox pos String {} = lookupVarName pos "String#"

-- | Rename a type.
rnType :: (Reader RnEnv :> es, IOE :> es, Reader Flag :> es, Error RenameError :> es) => Type (Malgo NewParse) -> Eff es (Type (Malgo Rename))
rnType (TyApp pos t ts) = TyApp pos <$> rnType t <*> traverse rnType ts
rnType (TyVar pos x)
  | isUpper (T.head x) = TyCon pos <$> lookupTypeName pos x
  | otherwise = TyVar pos <$> lookupTypeName pos x
rnType (TyArr pos t1 t2) = TyArr pos <$> rnType t1 <*> rnType t2
rnType (TyTuple pos ts) = TyTuple pos <$> traverse rnType ts
rnType (TyRecord pos kts) = TyRecord pos <$> traverse (bitraverse pure rnType) kts
rnType (TyBlock pos t) = TyArr pos (TyTuple pos []) <$> rnType t

-- | Rename a clause.
rnClause ::
  ( State RnState :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es,
    Reader Flag :> es,
    Error RenameError :> es
  ) =>
  Clause (Malgo NewParse) ->
  Eff es (Clause (Malgo Rename))
rnClause (Clause pos ps e) = do
  ps <- traverse resolveConP ps
  let vars = concatMap patVars ps
  -- パターンが束縛する変数に重複がないことを確認する
  -- TODO: throwError に置き換える
  when (anySame $ filter (/= "_") vars) $ errorOn pos "Same variables occurs in a pattern"
  vm <- zip vars . map (Qualified Implicit) <$> traverse resolveName vars
  local (insertVarIdent vm) $ Clause pos <$> traverse rnPat ps <*> rnExpr e
  where
    resolveConP :: Pat (Malgo NewParse) -> Eff es (Pat (Malgo NewParse))
    resolveConP (VarP pos name)
      | isUpper (T.head name) = pure $ ConP pos name []
      | otherwise = pure $ VarP pos name
    resolveConP (ConP pos name parameters) = do
      parameters' <- traverse resolveConP parameters
      pure $ ConP pos name parameters'
    resolveConP (TupleP pos parameters) = do
      parameters' <- traverse resolveConP parameters
      pure $ TupleP pos parameters'
    resolveConP (RecordP pos kvs) = do
      kvs' <- traverse (bitraverse pure resolveConP) kvs
      pure $ RecordP pos kvs'
    resolveConP (ListP pos parameters) = do
      parameters' <- traverse resolveConP parameters
      pure $ ListP pos parameters'
    resolveConP (UnboxedP pos x) = pure $ UnboxedP pos x
    resolveConP (BoxedP pos x) = pure $ BoxedP pos x

    patVars (VarP _ x) = [x]
    patVars (ConP _ _ xs) = concatMap patVars xs
    patVars (TupleP _ xs) = concatMap patVars xs
    patVars (RecordP _ kvs) = concatMap (patVars . snd) kvs
    patVars (ListP _ xs) = concatMap patVars xs
    patVars UnboxedP {} = []
    patVars BoxedP {} = []

-- | Rename a pattern.
rnPat :: (Reader RnEnv :> es, IOE :> es, Reader Flag :> es, Error RenameError :> es) => Pat (Malgo NewParse) -> Eff es (Pat (Malgo Rename))
rnPat (VarP pos x) = VarP pos <$> lookupVarName pos x
rnPat (ConP pos x xs) = ConP pos <$> lookupVarName pos x <*> traverse rnPat xs
rnPat (TupleP pos xs) = TupleP pos <$> traverse rnPat xs
rnPat (RecordP pos kvs) = RecordP pos <$> traverse (bitraverse pure rnPat) kvs
rnPat (ListP pos xs) = buildListP <$> lookupVarName pos "Nil" <*> lookupVarName pos "Cons" <*> traverse rnPat xs
  where
    buildListP nilName _ [] = ConP pos nilName []
    buildListP nilName consName (x : xs) = ConP pos consName [x, buildListP nilName consName xs]
rnPat (UnboxedP pos (String _)) = errorOn pos "String literal pattern is not supported"
rnPat (BoxedP pos (String _)) = errorOn pos "String literal pattern is not supported"
rnPat (UnboxedP pos x) = pure $ UnboxedP pos x
rnPat (BoxedP pos x) = ConP pos <$> lookupBox pos x <*> pure [UnboxedP pos (coerce x)]

-- | Rename statements in {}.
rnStmts ::
  ( State RnState :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es,
    Reader Flag :> es,
    Error RenameError :> es
  ) =>
  NonEmpty (Stmt (Malgo NewParse)) ->
  Eff es (NonEmpty (Stmt (Malgo Rename)))
rnStmts (NoBind x e :| []) = do
  e' <- rnExpr e
  pure $ NoBind x e' :| []
rnStmts (Let x v e :| []) = do
  e' <- rnExpr e
  v' <- resolveName v
  pure $ Let x v' e' :| []
rnStmts (NoBind x e :| s : ss) = do
  e' <- rnExpr e
  s' :| ss' <- rnStmts (s :| ss)
  pure $ NoBind x e' :| s' : ss'
rnStmts (Let x v e :| s : ss) = do
  e' <- rnExpr e
  v' <- resolveName v
  local (insertVarIdent [(v, Qualified Implicit v')]) do
    s' :| ss' <- rnStmts (s :| ss)
    pure $ Let x v' e' :| s' : ss'
rnStmts (With x (Just v) e :| s : ss) = do
  e <- rnExpr e
  ss <- rnExpr (Fn x $ Clause x [VarP x v] (Seq x $ s :| ss) :| [])
  pure $ NoBind x (Apply x e ss) :| []
rnStmts (With x Nothing e :| s : ss) = do
  e <- rnExpr e
  ss <- rnExpr (Fn x $ Clause x [] (Seq x $ s :| ss) :| [])
  pure $ NoBind x (Apply x e ss) :| []
rnStmts (With x _ _ :| []) = errorOn x "`with` statement cannnot appear in the last line of the sequence expression."

-- | Convert infix declarations to a Map. Infix for an undefined identifier is an error.
infixDecls :: (Reader RnEnv :> es, Error RenameError :> es) => [Decl (Malgo NewParse)] -> Eff es (Map RnId (Assoc, Int))
infixDecls ds =
  foldMapM ?? ds $ \case
    (Infix pos assoc order name) -> do
      name' <- lookupVarName pos name
      pure $ Map.singleton name' (assoc, order)
    _ -> pure mempty

-- | OpApp recombination.
-- Every OpApp in 'Malgo NewParsed' is treated as left associative.
-- 'mkOpApp' transforms it to actual associativity.
mkOpApp ::
  (IOE :> es, Reader Flag :> es) =>
  Range ->
  -- | Fixity of outer operator
  (Assoc, Int) ->
  -- | Outer operator
  RnId ->
  -- | Left expression as (x op y)
  Expr (Malgo 'Rename) ->
  -- | Right expression
  Expr (Malgo 'Rename) ->
  Eff es (Expr (Malgo 'Rename))
-- (e11 op1 e12) op2 e2
mkOpApp pos2 fix2 op2 (OpApp (pos1, fix1) op1 e11 e12) e2
  | nofix_error =
      errorOn pos1
        $ vsep
          [ "Precedence parsing error:",
            nest
              2
              ( "cannot mix"
                  <+> squotes (pretty op1)
                  <+> brackets (pretty fix1)
                  <+> "and"
                  <+> squotes (pretty op2)
                  <+> brackets (pretty fix2)
                  <+> "in the same infix expression"
              )
          ]
  | associate_right = do
      e' <- mkOpApp pos2 fix2 op2 e12 e2
      pure $ OpApp (pos1, fix1) op1 e11 e'
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2
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
mkOpApp pos fix op e1 e2 = pure $ OpApp (pos, fix) op e1 e2

-- | Generate toplevel environment.
genToplevelEnv :: (IOE :> es, Reader ModuleName :> es, State (Map ModuleName Interface) :> es, Workspace :> es, Error RenameError :> es) => [Decl (Malgo NewParse)] -> RnEnv -> Eff es RnEnv
genToplevelEnv (ds :: [Decl (Malgo NewParse)]) env = do
  execState env (traverse aux ds)
  where
    aux (ScDef pos x _) = do
      env <- gets @RnEnv (.resolvedVarIdentMap)
      when (x `elem` Map.keys env) do
        throwError $ DuplicateName pos x
      x' <- resolveGlobalName x
      modify $ insertVarIdent [(x, Qualified Implicit x')]
    aux ScSig {} = pass
    aux (DataDef pos x _ cs) = do
      env <- get @RnEnv
      when (x `elem` Map.keys env.resolvedTypeIdentMap) do
        throwError $ DuplicateName pos x
      unless (disjoint (map (view _2) cs) (Map.keys env.resolvedVarIdentMap)) do
        throwError $ DuplicateNames pos (map (view _2) cs `intersect` Map.keys env.resolvedVarIdentMap)
      x' <- resolveGlobalName x
      xs' <- traverse (resolveGlobalName . view _2) cs
      modify $ insertVarIdent (zip (map (view _2) cs) $ map (Qualified Implicit) xs')
      modify $ addConstructors xs'
      modify $ insertTypeIdent [(x, Qualified Implicit x')]
    aux (TypeSynonym pos x _ _) = do
      env <- get @RnEnv
      when (x `elem` Map.keys env.resolvedTypeIdentMap) do
        throwError $ DuplicateName pos x
      x' <- resolveGlobalName x
      modify $ insertTypeIdent [(x, Qualified Implicit x')]
    aux (Foreign pos x _) = do
      env <- get @RnEnv
      when (x `elem` Map.keys env.resolvedVarIdentMap) do
        throwError $ DuplicateName pos x
      x' <- resolveGlobalName x
      modify $ insertVarIdent [(x, Qualified Implicit x')]
    aux (Import _ modName' importList) = do
      -- If imported variables are already defined in the current module, shadow them.
      interface <- loadInterface modName'
      let varIdentAssoc =
            map
              (\name -> (name, externalFromInterface interface name))
              (exportedIdentList interface)
      let typeIdentAssoc =
            map
              (\name -> (name, externalFromInterface interface name))
              (exportedTypeIdentList interface)
      modify $ insertVarIdent (map (resolveImport modName' importList) varIdentAssoc)
      modify $ insertTypeIdent (map (resolveImport modName' importList) typeIdentAssoc)
      case importList of
        As moduleName -> modify \s -> s {moduleNames = Set.insert moduleName s.moduleNames}
        _ -> pass
    aux Infix {} = pass

resolveImport :: ModuleName -> ImportList -> (PsId, RnId) -> (PsId, Qualified RnId)
resolveImport _ All (psId, rnId) = (psId, Qualified Implicit rnId)
resolveImport modName (Selected implicits) (psId, rnId)
  | psId `elem` implicits = (psId, Qualified Implicit rnId)
  | otherwise = (psId, Qualified (Explicit modName) rnId)
resolveImport _ (As modNameAs) (psId, rnId) = (psId, Qualified (Explicit modNameAs) rnId)

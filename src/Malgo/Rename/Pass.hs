-- | Name resolution and simple desugar transformation
module Malgo.Rename.Pass (rename) where

import Control.Lens (view, (^.), _2)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (intersect)
import Data.List.Extra (anySame, disjoint)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, ask, local, runReader)
import Effectful.State.Static.Local (State, execState, get, gets, modify, put, runState)
import Koriel.Id
import Koriel.Lens
import Koriel.MonadUniq (Uniq)
import Koriel.Pretty
import Malgo.Interface
import Malgo.Prelude hiding (All)
import Malgo.Rename.RnEnv
import Malgo.Rename.RnState
import Malgo.Syntax
import Malgo.Syntax.Extension

-- | Entry point of this 'Malgo.Rename.Pass'
rename :: (Reader ModuleName :> es, Reader ModulePathList :> es, State (HashMap ModuleName Interface) :> es, State Uniq :> es, IOE :> es) => RnEnv -> Module (Malgo Parse) -> Eff es (Module (Malgo Rename), RnState)
rename builtinEnv (Module modName (ParsedDefinitions ds)) = do
  (ds', rnState) <- runState (RnState mempty HashSet.empty) $ runReader builtinEnv $ rnDecls ds
  pure (Module modName $ makeBindGroup ds', rnState)

-- renamer

-- | Rename toplevel declarations
rnDecls ::
  ( Reader ModuleName :> es,
    Reader ModulePathList :> es,
    Reader RnEnv :> es,
    State RnState :> es,
    State (HashMap ModuleName Interface) :> es,
    State Uniq :> es,
    IOE :> es
  ) =>
  [Decl (Malgo Parse)] ->
  Eff es [Decl (Malgo Rename)]
rnDecls ds = do
  -- RnEnvの生成
  rnEnv <- genToplevelEnv ds =<< ask
  local (const rnEnv) $ do
    -- RnStateの生成
    put =<< RnState <$> infixDecls ds <*> pure HashSet.empty
    -- 生成したRnEnv, RnStateの元でtraverse rnDecl ds
    traverse rnDecl ds

-- | Rename a toplevel declaration.
-- It is assumed that the top-level identifier defined in Decl has already been correctly registered in RnEnv.
-- The infix declaration is assumed to have already been interpreted and registered in RnState.
rnDecl ::
  ( State RnState :> es,
    State (HashMap ModuleName Interface) :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    Reader ModulePathList :> es,
    IOE :> es
  ) =>
  Decl (Malgo Parse) ->
  Eff es (Decl (Malgo Rename))
rnDecl (ScDef pos name expr) = ScDef pos <$> lookupVarName pos name <*> rnExpr expr
rnDecl (ScSig pos name typ) = do
  let tyVars = HashSet.toList $ getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (appendRnEnv resolvedTypeIdentMap (zip tyVars $ map (Qualified Implicit) tyVars')) $
    ScSig pos
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (DataDef pos name params cs) = do
  params' <- traverse (resolveName . snd) params
  local (appendRnEnv resolvedTypeIdentMap (zip (map snd params) (map (Qualified Implicit) params'))) $
    DataDef pos
      <$> lookupTypeName pos name
      <*> pure (zipWith (\(range, _) p' -> (range, p')) params params')
      <*> traverse (bitraverse (lookupVarName pos) (traverse rnType)) cs
rnDecl (TypeSynonym pos name params typ) = do
  params' <- traverse resolveName params
  local (appendRnEnv resolvedTypeIdentMap (zip params $ map (Qualified Implicit) params')) $
    TypeSynonym pos
      <$> lookupTypeName pos name
      <*> pure params'
      <*> rnType typ
rnDecl (Infix pos assoc prec name) = Infix pos assoc prec <$> lookupVarName pos name
rnDecl (Foreign pos name typ) = do
  let tyVars = HashSet.toList $ getTyVars typ
  tyVars' <- traverse resolveName tyVars
  local (appendRnEnv resolvedTypeIdentMap (zip tyVars $ map (Qualified Implicit) tyVars')) $
    Foreign (pos, name)
      <$> lookupVarName pos name
      <*> rnType typ
rnDecl (Import pos modName importList) = do
  interface <- loadInterface modName
  modify \s@RnState {..} ->
    s
      { _infixInfo = s._infixInfo <> interface.infixMap,
        _dependencies = HashSet.insert modName _dependencies <> interface.dependencies
      }
  pure $ Import pos modName importList

-- | Rename a expression.
-- In addition to name resolution, OpApp recombination based on infix declarations is also performed.
rnExpr ::
  ( State RnState :> es,
    State Uniq :> es,
    Reader RnEnv :> es,
    Reader ModuleName :> es,
    IOE :> es
  ) =>
  Expr (Malgo Parse) ->
  Eff es (Expr (Malgo Rename))
rnExpr (Var ((Qualified Implicit pos)) name) = Var pos <$> lookupVarName pos name
rnExpr (Var ((Qualified (Explicit modName) pos)) name) = Var pos <$> lookupQualifiedVarName pos modName name
rnExpr (Unboxed pos val) = pure $ Unboxed pos val
rnExpr (Boxed pos val) = do
  f <- lookupBox pos val
  pure $ Apply pos (Var pos f) (Unboxed pos $ toUnboxed val)
rnExpr (Apply pos e1 e2) = Apply pos <$> rnExpr e1 <*> rnExpr e2
rnExpr (OpApp pos op e1 e2) = do
  op' <- lookupVarName pos op
  e1' <- rnExpr e1
  e2' <- rnExpr e2
  mfixity <- HashMap.lookup op' <$> gets @RnState (._infixInfo)
  case mfixity of
    Just fixity -> mkOpApp pos fixity op' e1' e2'
    Nothing -> errorOn pos $ "No infix declaration:" <+> squotes (pretty op)
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
lookupBox :: (Reader RnEnv :> es, IOE :> es) => Range -> Literal x -> Eff es (Id ())
lookupBox pos Int32 {} = lookupVarName pos "Int32#"
lookupBox pos Int64 {} = lookupVarName pos "Int64#"
lookupBox pos Float {} = lookupVarName pos "Float#"
lookupBox pos Double {} = lookupVarName pos "Double#"
lookupBox pos Char {} = lookupVarName pos "Char#"
lookupBox pos String {} = lookupVarName pos "String#"

-- | Rename a type.
rnType :: (Reader RnEnv :> es, IOE :> es) => Type (Malgo Parse) -> Eff es (Type (Malgo Rename))
rnType (TyApp pos t ts) = TyApp pos <$> rnType t <*> traverse rnType ts
rnType (TyVar pos x) = TyVar pos <$> lookupTypeName pos x
rnType (TyCon pos x) = TyCon pos <$> lookupTypeName pos x
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
    IOE :> es
  ) =>
  Clause (Malgo Parse) ->
  Eff es (Clause (Malgo Rename))
rnClause (Clause pos ps e) = do
  let vars = concatMap patVars ps
  -- varsに重複がないことを確認
  when (anySame $ filter (/= "_") vars) $ errorOn pos "Same variables occurs in a pattern"
  vm <- zip vars . map (Qualified Implicit) <$> traverse resolveName vars
  local (appendRnEnv resolvedVarIdentMap vm) $ Clause pos <$> traverse rnPat ps <*> rnExpr e
  where
    patVars (VarP _ x) = [x]
    patVars (ConP _ _ xs) = concatMap patVars xs
    patVars (TupleP _ xs) = concatMap patVars xs
    patVars (RecordP _ kvs) = concatMap (patVars . snd) kvs
    patVars (ListP _ xs) = concatMap patVars xs
    patVars UnboxedP {} = []
    patVars BoxedP {} = []

-- | Rename a pattern.
rnPat :: (Reader RnEnv :> es, IOE :> es) => Pat (Malgo Parse) -> Eff es (Pat (Malgo Rename))
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
    IOE :> es
  ) =>
  NonEmpty (Stmt (Malgo Parse)) ->
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
  local (appendRnEnv resolvedVarIdentMap [(v, Qualified Implicit v')]) do
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
infixDecls :: (Reader RnEnv :> es, IOE :> es) => [Decl (Malgo 'Parse)] -> Eff es (HashMap RnId (Assoc, Int))
infixDecls ds =
  foldMapM ?? ds $ \case
    (Infix pos assoc order name) -> do
      name' <- lookupVarName pos name
      pure $ HashMap.singleton name' (assoc, order)
    _ -> pure mempty

-- | OpApp recombination.
-- Every OpApp in 'Malgo 'Parsed' is treated as left associative.
-- 'mkOpApp' transforms it to actual associativity.
mkOpApp ::
  MonadIO m =>
  Range ->
  -- | Fixity of outer operator
  (Assoc, Int) ->
  -- | Outer operator
  RnId ->
  -- | Left expression as (x op y)
  Expr (Malgo 'Rename) ->
  -- | Right expression
  Expr (Malgo 'Rename) ->
  m (Expr (Malgo 'Rename))
-- (e11 op1 e12) op2 e2
mkOpApp pos2 fix2 op2 (OpApp (pos1, fix1) op1 e11 e12) e2
  | nofix_error =
      errorOn pos1 $
        vsep
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
genToplevelEnv :: (IOE :> es, Reader ModuleName :> es, State (HashMap ModuleName Interface) :> es, Reader ModulePathList :> es) => [Decl (Malgo 'Parse)] -> RnEnv -> Eff es RnEnv
genToplevelEnv (ds :: [Decl (Malgo 'Parse)]) env = do
  execState env (traverse aux ds)
  where
    aux (ScDef pos x _) = do
      env <- gets @RnEnv (._resolvedVarIdentMap)
      when (x `elem` HashMap.keys env) do
        errorOn pos $ "Duplicate name:" <+> squotes (pretty x)
      x' <- resolveGlobalName x
      modify $ appendRnEnv resolvedVarIdentMap [(x, Qualified Implicit x')]
    aux ScSig {} = pass
    aux (DataDef pos x _ cs) = do
      env <- get @RnEnv
      when (x `elem` HashMap.keys (env ^. resolvedTypeIdentMap)) do
        errorOn pos $ "Duplicate name:" <+> squotes (pretty x)
      unless (disjoint (map (view _2) cs) (HashMap.keys (env ^. resolvedVarIdentMap))) do
        errorOn pos $
          "Duplicate name(s):"
            <+> sep
              (punctuate "," $ map (squotes . pretty) (map (view _2) cs `intersect` HashMap.keys (env ^. resolvedVarIdentMap)))
      x' <- resolveGlobalName x
      xs' <- traverse (resolveGlobalName . view _2) cs
      modify $ appendRnEnv resolvedVarIdentMap (zip (map (view _2) cs) $ map (Qualified Implicit) xs')
      modify $ appendRnEnv resolvedTypeIdentMap [(x, Qualified Implicit x')]
    aux (TypeSynonym pos x _ _) = do
      env <- get @RnEnv
      when (x `elem` HashMap.keys (env ^. resolvedTypeIdentMap)) do
        errorOn pos $ "Duplicate name:" <+> squotes (pretty x)
      x' <- resolveGlobalName x
      modify $ appendRnEnv resolvedTypeIdentMap [(x, Qualified Implicit x')]
    aux (Foreign pos x _) = do
      env <- get @RnEnv
      when (x `elem` HashMap.keys (env ^. resolvedVarIdentMap)) do
        errorOn pos $ "Duplicate name:" <+> squotes (pretty x)
      x' <- resolveGlobalName x
      modify $ appendRnEnv resolvedVarIdentMap [(x, Qualified Implicit x')]
    aux (Import _ modName' importList) = do
      interface <- loadInterface modName'
      let varIdentAssoc = HashMap.toList $ interface ^. resolvedVarIdentMap
      let typeIdentAssoc = HashMap.toList $ interface ^. resolvedTypeIdentMap
      modify $ appendRnEnv resolvedVarIdentMap (map (resolveImport modName' importList) varIdentAssoc)
      modify $ appendRnEnv resolvedTypeIdentMap (map (resolveImport modName' importList) typeIdentAssoc)
    aux Infix {} = pass

resolveImport :: ModuleName -> ImportList -> (PsId, RnId) -> (PsId, Qualified RnId)
resolveImport _ All (psId, rnId) = (psId, Qualified Implicit rnId)
resolveImport modName (Selected implicits) (psId, rnId)
  | psId `elem` implicits = (psId, Qualified Implicit rnId)
  | otherwise = (psId, Qualified (Explicit modName) rnId)
resolveImport _ (As modNameAs) (psId, rnId) = (psId, Qualified (Explicit modNameAs) rnId)

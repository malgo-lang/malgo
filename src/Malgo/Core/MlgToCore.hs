{-# LANGUAGE OverloadedStrings #-}

module Malgo.Core.MlgToCore (mlgToCore) where

import Control.Lens (At (at), Lens', ifor_, lens, over, traverseOf, traversed, use, view, (<>=), (<?=), (^.), _1, _2)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Koriel.Id
import Koriel.MonadUniq (HasUniqSupply)
import Koriel.Pretty
import Malgo.Core.Syntax
import Malgo.Prelude
import qualified Malgo.Syntax as S
import Malgo.Syntax.Extension (Malgo, MalgoPhase (Refine), RnId, WithPrefix (unwrapWithPrefix), removePrefix)
import Malgo.TypeCheck.TcEnv (TcEnv)
import qualified Malgo.TypeCheck.TcEnv as TcEnv
import qualified Malgo.TypeRep as T
import Relude.Unsafe (fromJust)

data DsEnv = DsEnv
  { -- | 脱糖の結果が詰め込まれるModule
    _buildingModule :: Module,
    -- | インターン済みのシンボル
    _interned :: HashMap (Int, IdSort) Name,
    -- _internedTyVar :: HashMap (Id T.Kind) Name,
    _tcEnv :: TcEnv
  }

buildingModule :: Lens' DsEnv Module
buildingModule = lens _buildingModule \e x -> e {_buildingModule = x}

interned :: Lens' DsEnv (HashMap (Int, IdSort) Name)
interned = lens _interned \e x -> e {_interned = x}

tcEnv :: Lens' DsEnv TcEnv
tcEnv = lens _tcEnv \e x -> e {_tcEnv = x}

mlgToCore :: (MonadIO m, MonadReader env m, HasUniqSupply env, MonadFail m) => TcEnv -> S.Module (Malgo 'Refine) -> m Module
mlgToCore _tcEnv (S.Module modName S.BindGroup {_scDefs, _dataDefs, _foreigns}) = evaluatingStateT
  DsEnv
    { _buildingModule = Module {_moduleName = modName, _variableDefinitions = [], _externalDefinitions = [], _typeDefinitions = []},
      _interned = mempty,
      _tcEnv = _tcEnv
    }
  do
    dsTypeDef =<< use (tcEnv . TcEnv.typeEnv)
    traverse_ dsForeign _foreigns
    traverse_ dsScDefs _scDefs
    use buildingModule

-- | インターンしたシンボル（Name）を返す
dsVarName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m) => RnId -> m Name
dsVarName old = do
  use (interned . at (old ^. idUniq, old ^. idSort)) >>= \case
    Just name -> pure name
    Nothing -> do
      scheme <-
        use (tcEnv . TcEnv.varEnv . at old) >>= \case
          Nothing -> errorDoc $ pPrint old <+> "is not defined"
          Just x -> pure x
      scheme <- dsScheme scheme
      name <- newIdOnName scheme old
      interned . at (old ^. idUniq, old ^. idSort) <?= name

dsFieldName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m) => Map (Id ()) T.Type -> Id () -> m (Id Type)
dsFieldName kts old = do
  use (interned . at (old ^. idUniq, old ^. idSort)) >>= \case
    Just name -> pure name
    Nothing -> do
      let ty = fromJust $ Map.lookup old kts
      ty <- dsType ty
      name <- newIdOnName ty old
      interned . at (old ^. idUniq, old ^. idSort) <?= name

-- | TcEnv.typeEnvのキーからModule.typeDefinitionsのキーへの変換
dsTypeName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => RnId -> m Name
dsTypeName old = do
  use (tcEnv . TcEnv.typeEnv . at old) >>= \case
    Just T.TypeDef {T._typeConstructor = T.TyCon con} -> dsTyVarName con
    Just _ ->
      use (interned . at (old ^. idUniq, old ^. idSort)) >>= \case
        Just name -> pure name
        Nothing -> do
          Just typeDef <- use (tcEnv . TcEnv.typeEnv . at old)
          let kind = T.kindOf typeDef
          kind <- dsType kind
          name <- newIdOnName kind old
          interned . at (old ^. idUniq, old ^. idSort) <?= name
    Nothing -> error "unreachable"

dsTyVarName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m) => Id T.Type -> m (Id Type)
dsTyVarName old = do
  use (interned . at (old ^. idUniq, old ^. idSort)) >>= \case
    Just name -> pure name
    Nothing -> do
      kind <- dsType (old ^. idMeta)
      name <- newIdOnName kind old
      interned . at (old ^. idUniq, old ^. idSort) <?= name

dsScheme :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m) => T.Scheme T.Type -> m Type
dsScheme (T.Forall [] ty) = dsType ty
dsScheme (T.Forall (p : ps) ty) = TyForall <$> dsTyVarName p <*> dsScheme (T.Forall ps ty)

dsType :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m) => T.Type -> m Type
dsType (T.TyConApp (T.TyCon con) ts) = do
  con <- dsTyVarName con
  TyConApp (TyCon con) <$> traverse dsType ts
dsType (T.TyConApp (T.TyTuple _) ts) = do
  ts <- traverse dsType ts
  pure $ TyConApp (TupleC $ map kindOf ts) ts
dsType T.TyApp {} = error "unreachable"
dsType (T.TyVar v) = TyVar <$> dsTyVarName v
dsType (T.TyCon con) = do
  con <- dsTyVarName con
  pure $ TyConApp (TyCon con) []
dsType (T.TyArr t1 t2) = TyFun <$> dsType t1 <*> dsType t2
dsType (T.TyPrim p) = pure $ TyPrim p
dsType (T.TYPE rep) = TYPE <$> dsRep rep
  where
    dsRep (T.Rep rep) = pure rep
    dsRep _ = error "invalid Rep"

dsForeign :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => S.Foreign (Malgo 'Refine) -> m ()
dsForeign (_, name, _) = do
  name <- dsVarName name
  buildingModule . externalDefinitions <>= [(name, toString $ name ^. idName)]

dsScDefs :: (MonadState DsEnv f, MonadIO f, HasUniqSupply env, MonadReader env f, MonadFail f) => [S.ScDef (Malgo 'Refine)] -> f ()
dsScDefs = traverse_ dsScDef

dsScDef :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => S.ScDef (Malgo 'Refine) -> m ()
dsScDef (_, name, expr) = do
  name <- dsVarName name
  expr <- dsExp expr
  buildingModule . variableDefinitions <>= [(name, expr)]

dsExp :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => S.Exp (Malgo 'Refine) -> m Exp
dsExp (S.Var _ v) = Var <$> dsVarName (removePrefix v)
dsExp (S.Unboxed _ (S.Int32 x)) = pure $ Unboxed $ Int32 x
dsExp (S.Unboxed _ (S.Int64 x)) = pure $ Unboxed $ Int64 x
dsExp (S.Unboxed _ (S.Float x)) = pure $ Unboxed $ Float x
dsExp (S.Unboxed _ (S.Double x)) = pure $ Unboxed $ Double x
dsExp (S.Unboxed _ (S.Char x)) = pure $ Unboxed $ Char x
dsExp (S.Unboxed _ (S.String x)) = pure $ Unboxed $ String x
dsExp e@S.Apply {} = do
  let (f, args) = viewApply e
  f <- dsExp f
  args <- traverse dsExp args
  pure $ Apply f args
dsExp (S.Fn _ clauses@(S.Clause _ ps _ :| _)) = do
  ps' <- traverse (\p -> newInternalId (patToName p) =<< dsType (T.typeOf p)) ps
  clauses <- toList <$> traverse dsClause clauses
  pure $ Fn ps' (Match (map Var ps') clauses)
  where
    patToName (S.VarP _ v) = v ^. idName
    patToName (S.ConP _ c _) = T.toLower $ c ^. idName
    patToName (S.TupleP _ _) = "tuple"
    patToName (S.RecordP _ _) = "record"
    patToName (S.ListP _ _) = "list"
    patToName (S.UnboxedP _ _) = "unboxed"
    patToName (S.BoxedP _ _) = "boxed"
    dsClause (S.Clause _ ps e) = do
      ps <- traverse dsPat ps
      e <- dsExp e
      pure $ Clause ps e
    dsPat (S.VarP _ v) = VarP <$> dsVarName v
    dsPat (S.ConP _ c ps) = ConP <$> dsVarName c <*> traverse dsPat ps
dsExp (S.Tuple _ es) = Tuple <$> traverse dsExp es
dsExp (S.Record (T.typeOf -> T.TyRecord kts) kes) =
  Record . HashMap.fromList <$> traverse (bitraverse (dsFieldName kts . removePrefix) dsExp) kes
dsExp S.Record {} = error "unreachable"
dsExp (S.RecordAccess (T.typeOf -> T.TyArr (T.TyRecord kts) _) field) =
  RecordAccess <$> dsRecordType kts <*> dsFieldName kts (removePrefix field)
dsExp S.RecordAccess {} = error "unreachable"
dsExp (S.Seq _ stmts) = dsSeq stmts
  where
    dsSeq (S.NoBind _ e :| []) = dsExp e
    dsSeq (S.NoBind _ e :| s : ss) = do
      hole <- newInternalId "_" =<< dsType (T.typeOf e)
      Let hole <$> dsExp e <*> dsSeq (s :| ss)
    dsSeq (S.Let _ _ e :| []) = dsExp e
    dsSeq (S.Let _ name e :| s : ss) = do
      name <- dsVarName name
      Let name <$> dsExp e <*> dsSeq (s :| ss)
dsExp (S.Parens _ e) = dsExp e

dsRecordType :: (MonadReader env f, HasUniqSupply env, MonadIO f, MonadState DsEnv f) => Map (Id ()) T.Type -> f (HashMap (Id Type) Type)
dsRecordType recordType = do
  let recordkts = Map.toList recordType
  HashMap.fromList <$> traverse (\(k, t) -> (,) <$> dsFieldName recordType k <*> dsType t) recordkts

viewApply :: S.Exp x -> (S.Exp x, [S.Exp x])
viewApply (S.Apply _ e1 e2) = over _2 (<> [e2]) $ viewApply e1
viewApply e = (e, [])

dsTypeDef :: (MonadState DsEnv m, MonadIO m, MonadReader env m, HasUniqSupply env, MonadFail m) => HashMap RnId (T.TypeDef T.Type) -> m ()
dsTypeDef typeDefs = ifor_ typeDefs \name typeDef -> do
  name' <- dsTypeName name
  typeDef' <- dsTypeDef' typeDef
  buildingModule . typeDefinitions <>= [(name', typeDef')]
  where
    dsTypeDef' T.TypeDef {..} = do
      _parameters <- traverse dsTyVarName _typeParameters
      _constructors <- traverseOf traversed (dsVarName . view _1) _valueConstructors
      pure TypeDef {..}

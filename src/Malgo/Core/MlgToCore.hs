{-# LANGUAGE OverloadedStrings #-}

module Malgo.Core.MlgToCore (mlgToCore) where

import Control.Lens (At (at), Lens', ifor_, lens, over, traverseOf, traversed, use, view, (<?=), (^.), _1, _2, (<>=))
import Koriel.Id
import Koriel.MonadUniq (HasUniqSupply)
import Koriel.Pretty
import Malgo.Core.Syntax
import Malgo.Prelude
import qualified Malgo.Syntax as S
import Malgo.Syntax.Extension (Malgo, MalgoPhase (Refine), RnId, removePrefix)
import Malgo.TypeCheck.TcEnv (TcEnv)
import qualified Malgo.TypeCheck.TcEnv as TcEnv
import qualified Malgo.TypeRep as T
import Text.Pretty.Simple (pShow)
import qualified Data.Text as T

data DsEnv = DsEnv
  { -- | 脱糖の結果が詰め込まれるModule
    _buildingModule :: Module,
    -- | インターン済みのシンボル
    _interned :: HashMap Int Name,
    -- _internedTyVar :: HashMap (Id T.Kind) Name,
    _tcEnv :: TcEnv
  }

buildingModule :: Lens' DsEnv Module
buildingModule = lens _buildingModule \e x -> e {_buildingModule = x}

interned :: Lens' DsEnv (HashMap Int Name)
interned = lens _interned \e x -> e {_interned = x}

tcEnv :: Lens' DsEnv TcEnv
tcEnv = lens _tcEnv \e x -> e {_tcEnv = x}

mlgToCore :: (MonadIO m, MonadReader env m, HasUniqSupply env, MonadFail m) => TcEnv -> S.Module (Malgo 'Refine) -> m Module
mlgToCore _tcEnv (S.Module modName S.BindGroup {_scDefs, _dataDefs}) = evaluatingStateT
  DsEnv
    { _buildingModule = Module {_moduleName = modName, _variableDefinitions = [], _externalDefinitions = [], _typeDefinitions = []},
      _interned = mempty,
      _tcEnv = _tcEnv
    }
  do
    dsTypeDef =<< use (tcEnv . TcEnv.typeEnv)
    traverse_ dsScDefs _scDefs
    use buildingModule

-- | インターンしたシンボル（Name）を返す
dsVarName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => RnId -> m Name
dsVarName old = do
  use (interned . at (old ^. idUniq)) >>= \case
    Just name -> pure name
    Nothing -> do
      Just scheme <- use (tcEnv . TcEnv.varEnv . at old)
      scheme <- dsScheme scheme
      name <- newIdOnName scheme old
      interned . at (old ^. idUniq) <?= name

-- | TcEnv.typeEnvのキーからModule.typeDefinitionsのキーへの変換
dsTypeName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m, MonadFail m) => RnId -> m Name
dsTypeName old = do
  use (tcEnv . TcEnv.typeEnv . at old) >>= \case
    Just T.TypeDef {T._typeConstructor = T.TyCon con} -> dsTyVarName con
    Just _ ->
      use (interned . at (old ^. idUniq)) >>= \case
        Just name -> pure name
        Nothing -> do
          Just typeDef <- use (tcEnv . TcEnv.typeEnv . at old)
          let kind = T.kindOf typeDef
          kind <- dsType kind
          name <- newIdOnName kind old
          interned . at (old ^. idUniq) <?= name
    Nothing -> error "unreachable"

dsTyVarName :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m) => Id T.Type -> m (Id Type)
dsTyVarName old = do
  use (interned . at (old ^. idUniq)) >>= \case
    Just name -> pure name
    Nothing -> do
      kind <- dsType (old ^. idMeta)
      name <- newIdOnName kind old
      interned . at (old ^. idUniq) <?= name

dsScheme :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m) => T.Scheme T.Type -> m Type
dsScheme (T.Forall [] ty) = dsType ty
dsScheme (T.Forall (p : ps) ty) = TyForall <$> dsTyVarName p <*> dsScheme (T.Forall ps ty)

dsType :: (MonadState DsEnv m, MonadIO m, HasUniqSupply env, MonadReader env m) => T.Type -> m Type
dsType (T.TyCon con) = do
  con <- dsTyVarName con
  pure $ TyConApp (TyCon con) []
dsType (T.TyArr t1 t2) = TyFun <$> dsType t1 <*> dsType t2
dsType (T.TyPrim p) = pure $ TyPrim p
dsType (T.TYPE rep) = TYPE <$> dsRep rep
  where
    dsRep (T.Rep rep) = pure rep
    dsRep _ = error "invalid Rep"
dsType t = errorDoc $ "not implemented:" <+> pPrint t

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
dsExp (S.Seq _ stmts) = dsSeq stmts
  where
    dsSeq (S.NoBind _ e :| []) = dsExp e
    dsSeq (S.NoBind _ e :| s : ss) = do
      hole <- newInternalId "_" =<< dsType (T.typeOf e)
      Let hole <$> dsExp e <*>  dsSeq (s :| ss)
dsExp e = error $ fromLazy $ "not implemented:\n" <> pShow e

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

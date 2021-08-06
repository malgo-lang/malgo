-- | Malgo.Refine.Pass is the final AST-to-AST pass.
-- This pass will remove unnecessary Parens and OpApp, and transforms Type annotation's representation to Static one.
module Malgo.Refine.Pass where

import Data.Kind (Constraint)
import Malgo.Prelude
import Malgo.Refine.RefineEnv
import qualified Malgo.Refine.Space as Space
import Malgo.Syntax hiding (TyArr, Type)
import qualified Malgo.Syntax as Syn
import Malgo.Syntax.Extension
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep.Static
import qualified RIO.List as List
import qualified RIO.NonEmpty as NonEmpty

type TypeChecked t x = (x ~ Malgo 'TypeCheck) :: Constraint

refine :: (TypeChecked t x, MonadIO m) => TcEnv -> Module x -> m (Module (Malgo 'Refine))
refine tcEnv Module {_moduleName, _moduleDefinition} =
  Module _moduleName <$> runReaderT (refineBindGroup _moduleDefinition) (buildRefineEnv tcEnv)

refineBindGroup :: forall t x m. (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => BindGroup x -> m (BindGroup (Malgo 'Refine))
refineBindGroup BindGroup {..} =
  BindGroup
    <$> traverse (traverse refineScDef) _scDefs
    <*> traverse refineScSig _scSigs
    <*> traverse refineDataDef _dataDefs
    <*> traverse refineTypeSynonym _typeSynonyms
    <*> traverse refineForeign _foreigns
    <*> traverse (refineImport @t @x) _imports

refineScDef :: (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => ScDef x -> m (ScDef (Malgo 'Refine))
refineScDef (x, name, expr) = (over ann toType x,name,) <$> refineExp expr

refineExp :: (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => Exp x -> m (Exp (Malgo 'Refine))
refineExp (Var x m v) = pure $ Var (over ann toType x) m v
refineExp (Unboxed x u) = pure $ Unboxed (over ann toType x) u
refineExp (Apply x e1 e2) = Apply (over ann toType x) <$> refineExp e1 <*> refineExp e2
refineExp (OpApp x op e1 e2) = do
  let x' = over ann toType $ over value (view _1) x
  e1' <- refineExp e1
  e2' <- refineExp e2
  let applyType = TyArr (typeOf e2') (x' ^. ann)
  let opType = TyArr (typeOf e1') applyType
  pure $ Apply x' (Apply (x' & ann .~ applyType) (Var (x' & ann .~ opType) Nothing op) e1') e2'
refineExp (Fn x cs) = do
  let x' = over ann toType x
  cs' <- traverse refineClause cs
  env <- ask
  let typeSpaces = map (Space.normalize . Space.space env) $ x' ^. ann . to splitTyArr . _1
  let patSpaces = map (Space.normalize . Space.buildUnion) $ List.transpose $ NonEmpty.toList $ fmap (clauseSpace env) cs'
  exhaustive <- fmap Space.normalize <$> zipWithM Space.subtract typeSpaces patSpaces
  when (any (/= Space.Empty) exhaustive) $
    errorOn (x ^. value) "Pattern is not exhaustive"
  pure $ Fn x' cs'
  where
    clauseSpace env (Clause _ ps _) = map (Space.space env) ps
refineExp (Tuple x es) = Tuple (over ann toType x) <$> traverse refineExp es
refineExp (Record x kvs) = Record (over ann toType x) <$> traverseOf (traversed . _2) refineExp kvs
refineExp (Force x e) = Force (over ann toType x) <$> refineExp e
refineExp (RecordAccess x label) = pure $ RecordAccess (over ann toType x) label
refineExp (Parens _ e) = refineExp e

refineClause :: (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => Clause x -> m (Clause (Malgo 'Refine))
refineClause (Clause x ps es) = Clause (over ann toType x) <$> traverse refinePat ps <*> traverse refineStmt es

refineStmt :: (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => Stmt x -> m (Stmt (Malgo 'Refine))
refineStmt (Let x v e) = Let x v <$> refineExp e
refineStmt (NoBind x e) = NoBind x <$> refineExp e

refinePat :: (TypeChecked t x, MonadReader RefineEnv m) => Pat x -> m (Pat (Malgo 'Refine))
refinePat (VarP x v) = pure $ VarP (over ann toType x) v
refinePat (ConP x c ps) = ConP (over ann toType x) c <$> traverse refinePat ps
refinePat (TupleP x ps) = TupleP (over ann toType x) <$> traverse refinePat ps
refinePat (RecordP x kps) = RecordP (over ann toType x) <$> traverseOf (traversed . _2) refinePat kps
refinePat (UnboxedP x u) = pure $ UnboxedP (over ann toType x) u

refineScSig :: (TypeChecked t x, MonadReader RefineEnv m) => ScSig x -> m (ScSig (Malgo 'Refine))
refineScSig (x, name, ty) = (x,name,) <$> refineType ty

refineType :: (TypeChecked t x, MonadReader RefineEnv m) => Syn.Type x -> m (Syn.Type (Malgo 'Refine))
refineType (Syn.TyApp x t ts) = Syn.TyApp x <$> refineType t <*> traverse refineType ts
refineType (Syn.TyVar x v) = pure $ Syn.TyVar x v
refineType (Syn.TyCon x v) = pure $ Syn.TyCon x v
refineType (Syn.TyArr x t1 t2) = Syn.TyArr x <$> refineType t1 <*> refineType t2
refineType (Syn.TyTuple x ts) = Syn.TyTuple x <$> traverse refineType ts
refineType (Syn.TyRecord x kts) = Syn.TyRecord x <$> traverseOf (traversed . _2) refineType kts
refineType (Syn.TyLazy x t) = Syn.TyLazy x <$> refineType t

refineDataDef :: (TypeChecked t x, MonadReader RefineEnv m) => DataDef x -> m (DataDef (Malgo 'Refine))
refineDataDef (x, name, ps, cons) = (x,name,ps,) <$> traverse (_2 $ traverse refineType) cons

refineTypeSynonym :: (TypeChecked t x, MonadReader RefineEnv m) => TypeSynonym x -> m (TypeSynonym (Malgo 'Refine))
refineTypeSynonym (x, name, ps, typ) = (x,name,ps,) <$> refineType typ

refineForeign :: (TypeChecked t x, MonadReader RefineEnv m) => Foreign x -> m (Foreign (Malgo 'Refine))
refineForeign (x, name, ty) = (over ann toType x,name,) <$> refineType ty

refineImport :: (TypeChecked t x, MonadReader RefineEnv m) => Import x -> m (Import (Malgo 'Refine))
refineImport (x, modName, importList) = pure (x, modName, importList)

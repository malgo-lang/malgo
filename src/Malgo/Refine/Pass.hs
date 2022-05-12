-- | Malgo.Refine.Pass is the final AST-to-AST pass.
-- This pass will remove unnecessary Parens and OpApp, and transforms Type annotation's representation to Static one.
module Malgo.Refine.Pass where

import Control.Lens (over, to, traverseOf, traversed, view, (.~), (^.), _1, _2)
import qualified Data.List.NonEmpty as NonEmpty
import Koriel.Lens (HasAnn (ann), HasValue (value))
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Refine.RefineEnv
import qualified Malgo.Refine.Space as Space
import Malgo.Syntax hiding (TyArr, Type)
import qualified Malgo.Syntax as Syn
import Malgo.Syntax.Extension
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep

type TypeChecked t x = (x ~ Malgo 'TypeCheck) :: Constraint

refine :: (TypeChecked t x, MonadIO m, MonadReader env m, HasMalgoEnv env) => TcEnv -> Module x -> m (Module (Malgo 'Refine))
refine tcEnv Module {_moduleName, _moduleDefinition} = do
  malgoEnv <- view malgoEnv
  Module _moduleName <$> runReaderT (refineBindGroup _moduleDefinition) (buildRefineEnv malgoEnv tcEnv)

refineBindGroup :: forall t x m. (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => BindGroup x -> m (BindGroup (Malgo 'Refine))
refineBindGroup BindGroup {..} = do
  _scDefs <- traverse (traverse refineScDef) _scDefs
  _scSigs <- traverse refineScSig _scSigs
  _dataDefs <- traverse refineDataDef _dataDefs
  _typeSynonyms <- traverse refineTypeSynonym _typeSynonyms
  _foreigns <- traverse refineForeign _foreigns
  _imports <- traverse (refineImport @t @x) _imports
  pure BindGroup {..}

refineScDef :: (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => ScDef x -> m (ScDef (Malgo 'Refine))
refineScDef (x, name, expr) = (x,name,) <$> refineExp expr

refineExp :: (MonadReader RefineEnv m, MonadIO m) => Exp (Malgo 'TypeCheck) -> m (Exp (Malgo 'Refine))
refineExp (Var x v) = pure $ Var x v
refineExp (Unboxed x u) = pure $ Unboxed x u
refineExp (Apply x e1 e2) = Apply x <$> refineExp e1 <*> refineExp e2
refineExp (OpApp x op e1 e2) = do
  e1' <- refineExp e1
  e2' <- refineExp e2
  let applyType = TyArr (typeOf e2') (x ^. ann)
  let opType = TyArr (typeOf e1') applyType
  let x' = x {_value = fst $ x ^. value}
  pure $ Apply x' (Apply (x' & ann .~ applyType) (Var (x' & ann .~ opType) (NoPrefix op)) e1') e2'
refineExp (Fn x cs) = do
  cs' <- traverse refineClause cs
  env <- ask
  let typeSpaces = map (Space.normalize . Space.space env) $ x ^. ann . to splitTyArr . _1
  let patSpaces = map (Space.normalize . Space.buildUnion) $ transpose $ NonEmpty.toList $ fmap (clauseSpace env) cs'
  exhaustive <- fmap Space.normalize <$> zipWithM Space.subtract typeSpaces patSpaces
  isEmptys <- traverse Space.equalEmpty exhaustive
  when (any not isEmptys) $
    errorOn (x ^. value) $ "Pattern is not exhaustive:" <+> pPrint exhaustive
  pure $ Fn x cs'
  where
    clauseSpace env (Clause _ ps _) = map (Space.space env) ps
refineExp (Tuple x es) = Tuple x <$> traverse refineExp es
refineExp (Record x kvs) = Record x <$> traverseOf (traversed . _2) refineExp kvs
refineExp (RecordAccess x label) = pure $ RecordAccess x label
refineExp (Seq x ss) = Seq x <$> traverse refineStmt ss
refineExp (Parens _ e) = refineExp e

refineClause :: (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => Clause x -> m (Clause (Malgo 'Refine))
refineClause (Clause x ps e) = Clause x <$> traverse refinePat ps <*> refineExp e

refineStmt :: (TypeChecked t x, MonadReader RefineEnv m, MonadIO m) => Stmt x -> m (Stmt (Malgo 'Refine))
refineStmt (Let x v e) = Let x v <$> refineExp e
refineStmt (NoBind x e) = NoBind x <$> refineExp e

refinePat :: (TypeChecked t x, MonadReader RefineEnv m) => Pat x -> m (Pat (Malgo 'Refine))
refinePat (VarP x v) = pure $ VarP x v
refinePat (ConP x c ps) = ConP x c <$> traverse refinePat ps
refinePat (TupleP x ps) = TupleP x <$> traverse refinePat ps
refinePat (RecordP x kps) = RecordP x <$> traverseOf (traversed . _2) refinePat kps
refinePat (UnboxedP x u) = pure $ UnboxedP x u

refineScSig :: (TypeChecked t x, MonadReader RefineEnv m) => ScSig x -> m (ScSig (Malgo 'Refine))
refineScSig (x, name, ty) = (x,name,) <$> refineType ty

refineType :: (TypeChecked t x, MonadReader RefineEnv m) => Syn.Type x -> m (Syn.Type (Malgo 'Refine))
refineType (Syn.TyApp x t ts) = Syn.TyApp x <$> refineType t <*> traverse refineType ts
refineType (Syn.TyVar x v) = pure $ Syn.TyVar x v
refineType (Syn.TyCon x v) = pure $ Syn.TyCon x v
refineType (Syn.TyArr x t1 t2) = Syn.TyArr x <$> refineType t1 <*> refineType t2
refineType (Syn.TyTuple x ts) = Syn.TyTuple x <$> traverse refineType ts
refineType (Syn.TyRecord x kts) = Syn.TyRecord x <$> traverseOf (traversed . _2) refineType kts

refineDataDef :: (TypeChecked t x, MonadReader RefineEnv m) => DataDef x -> m (DataDef (Malgo 'Refine))
refineDataDef (x, name, ps, cons) = (x,name,ps,) <$> traverse (_2 $ traverse refineType) cons

refineTypeSynonym :: (TypeChecked t x, MonadReader RefineEnv m) => TypeSynonym x -> m (TypeSynonym (Malgo 'Refine))
refineTypeSynonym (x, name, ps, typ) = (x,name,ps,) <$> refineType typ

refineForeign :: (TypeChecked t x, MonadReader RefineEnv m) => Foreign x -> m (Foreign (Malgo 'Refine))
refineForeign (x, name, ty) = (x,name,) <$> refineType ty

refineImport :: (TypeChecked t x, MonadReader RefineEnv m) => Import x -> m (Import (Malgo 'Refine))
refineImport (x, modName, importList) = pure (x, modName, importList)

-- | Language.Malgo.Refine.Pass is the final AST-to-AST pass.
-- This pass will remove unnecessary Parens and OpApp, and transforms Type annotation's representation to Static one.
module Language.Malgo.Refine.Pass where

import Data.Kind (Constraint)
import Language.Malgo.Prelude
import Language.Malgo.Syntax hiding (TyArr)
import qualified Language.Malgo.Syntax as Syn
import Language.Malgo.Syntax.Extension
import Language.Malgo.TypeRep.Static

type TypeChecked t x = (x ~ Malgo 'TypeCheck) :: Constraint

refine :: (TypeChecked t x, Monad m) => Module x -> m (Module (Malgo 'Refine))
refine Module {_moduleName, _moduleDefinition} = Module _moduleName <$> refineBindGroup _moduleDefinition

refineBindGroup :: forall t x m. (TypeChecked t x, Monad m) => BindGroup x -> m (BindGroup (Malgo 'Refine))
refineBindGroup BindGroup {..} =
  BindGroup
    <$> traverse (traverse refineScDef) _scDefs
    <*> traverse refineScSig _scSigs
    <*> traverse refineDataDef _dataDefs
    <*> traverse refineTypeSynonym _typeSynonyms
    <*> traverse refineForeign _foreigns
    <*> traverse (refineImport @t @x) _imports

refineScDef :: (TypeChecked t x, Monad m) => ScDef x -> m (ScDef (Malgo 'Refine))
refineScDef (x, name, expr) = (over ann toType x,name,) <$> refineExp expr

refineExp :: (TypeChecked t x, Monad m) => Exp x -> m (Exp (Malgo 'Refine))
refineExp (Var x v) = pure $ Var (over ann toType x) v
refineExp (Con x c) = pure $ Con (over ann toType x) c
refineExp (Unboxed x u) = pure $ Unboxed (over ann toType x) u
refineExp (Apply x e1 e2) = Apply (over ann toType x) <$> refineExp e1 <*> refineExp e2
refineExp (OpApp x op e1 e2) = do
  let x' = over ann toType $ over value (view _1) x
  e1' <- refineExp e1
  e2' <- refineExp e2
  applyType <- TyArr <$> typeOf e2' <*> pure (x' ^. ann)
  opType <- TyArr <$> typeOf e1' <*> pure applyType
  pure $ Apply x' (Apply (x' & ann .~ applyType) (Var (x' & ann .~ opType) op) e1') e2'
refineExp (Fn x cs) = Fn (over ann toType x) <$> traverse refineClause cs
refineExp (Tuple x es) = Tuple (over ann toType x) <$> traverse refineExp es
refineExp (Record x kvs) = Record (over ann toType x) <$> traverseOf (traversed . _2) refineExp kvs
refineExp (Force x e) = Force (over ann toType x) <$> refineExp e
refineExp (RecordAccess x label) = pure $ RecordAccess (over ann toType x) label
refineExp (Parens _ e) = refineExp e

refineClause :: (TypeChecked t x, Monad m) => Clause x -> m (Clause (Malgo 'Refine))
refineClause (Clause x ps es) = Clause (over ann toType x) <$> traverse refinePat ps <*> traverse refineStmt es

refineStmt :: (TypeChecked t x, Monad m) => Stmt x -> m (Stmt (Malgo 'Refine))
refineStmt (Let x v e) = Let x v <$> refineExp e
refineStmt (NoBind x e) = NoBind x <$> refineExp e

refinePat :: (TypeChecked t x, Applicative m) => Pat x -> m (Pat (Malgo 'Refine))
refinePat (VarP x v) = pure $ VarP (over ann toType x) v
refinePat (ConP x c ps) = ConP (over ann toType x) c <$> traverse refinePat ps
refinePat (TupleP x ps) = TupleP (over ann toType x) <$> traverse refinePat ps
refinePat (RecordP x kps) = RecordP (over ann toType x) <$> traverseOf (traversed . _2) refinePat kps
refinePat (UnboxedP x u) = pure $ UnboxedP (over ann toType x) u

refineScSig :: (TypeChecked t x, Applicative m) => ScSig x -> m (ScSig (Malgo 'Refine))
refineScSig (x, name, ty) = (x,name,) <$> refineType ty

refineType :: (TypeChecked t x, Applicative m) => Syn.Type x -> m (Syn.Type (Malgo 'Refine))
refineType (Syn.TyApp x t ts) = Syn.TyApp x <$> refineType t <*> traverse refineType ts
refineType (Syn.TyVar x v) = pure $ Syn.TyVar x v
refineType (Syn.TyCon x v) = pure $ Syn.TyCon x v
refineType (Syn.TyArr x t1 t2) = Syn.TyArr x <$> refineType t1 <*> refineType t2
refineType (Syn.TyTuple x ts) = Syn.TyTuple x <$> traverse refineType ts
refineType (Syn.TyRecord x kts) = Syn.TyRecord x <$> traverseOf (traversed . _2) refineType kts
refineType (Syn.TyLazy x t) = Syn.TyLazy x <$> refineType t

refineDataDef :: (TypeChecked t x, Applicative m) => DataDef x -> m (DataDef (Malgo 'Refine))
refineDataDef (x, name, ps, cons) = (x,name,ps,) <$> traverse (_2 $ traverse refineType) cons

refineTypeSynonym :: (TypeChecked t x, Applicative m) => TypeSynonym x -> m (TypeSynonym (Malgo 'Refine))
refineTypeSynonym (x, name, ps, typ) = (x,name,ps,) <$> refineType typ

refineForeign :: (TypeChecked t x, Applicative m) => Foreign x -> m (Foreign (Malgo 'Refine))
refineForeign (x, name, ty) = (over ann toType x,name,) <$> refineType ty

refineImport :: (TypeChecked t x, Applicative m) => Import x -> m (Import (Malgo 'Refine))
refineImport (x, modName) = pure (x, modName)

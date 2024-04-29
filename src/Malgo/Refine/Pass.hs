-- | Malgo.Refine.Pass is the final AST-to-AST pass.
-- This pass will remove unnecessary Parens and OpApp, and transforms Type annotation's representation to Static one.
module Malgo.Refine.Pass (refine) where

import Control.Lens (_3)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Effectful
import Effectful.Reader.Static
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Infer.TypeRep qualified as T
import Malgo.Prelude
import Malgo.Refine.RefineEnv
import Malgo.Refine.Space qualified as Space
import Malgo.Syntax hiding (TyArr, Type)
import Malgo.Syntax qualified as Syn
import Malgo.Syntax.Extension

refine :: (IOE :> es, Reader Flag :> es) => TcEnv -> Module (Malgo Infer) -> Eff es (Module (Malgo Refine))
refine tcEnv Module {..} = do
  Module moduleName <$> runReader (buildRefineEnv tcEnv) (refineBindGroup moduleDefinition)

refineBindGroup :: (Reader RefineEnv :> es, IOE :> es, Reader Flag :> es) => BindGroup (Malgo Infer) -> Eff es (BindGroup (Malgo Refine))
refineBindGroup BindGroup {..} = do
  _scDefs <- traverse (traverse refineScDef) _scDefs
  _scSigs <- traverse refineScSig _scSigs
  _dataDefs <- traverse refineDataDef _dataDefs
  _typeSynonyms <- traverse refineTypeSynonym _typeSynonyms
  _foreigns <- traverse refineForeign _foreigns
  _imports <- traverse refineImport _imports
  pure BindGroup {..}

refineScDef :: (Reader RefineEnv :> es, IOE :> es, Reader Flag :> es) => ScDef (Malgo Infer) -> Eff es (ScDef (Malgo Refine))
refineScDef (x, name, expr) = (x,name,) <$> refineExpr expr

refineExpr :: (Reader RefineEnv :> es, IOE :> es, Reader Flag :> es) => Expr (Malgo Infer) -> Eff es (Expr (Malgo Refine))
refineExpr (Var x v) = do
  vScheme <- asks @RefineEnv ((.signatureMap) >>> HashMap.lookup v)
  case vScheme of
    Nothing -> pass
    Just (Forall _ originalType) -> do
      let instantiatedType = x.annotated
      checkValidInstantiation originalType instantiatedType
  pure $ Var x v
  where
    checkValidInstantiation (T.TyVar v) (TyPrim p) = errorOn x.value $ "Invalid instantiation:" <+> "'" <> pretty v <> "'" <+> "can't be instantiated with" <+> pretty p
    checkValidInstantiation (T.TyVar _) _ = pass
    checkValidInstantiation (T.TyApp t1 t2) (T.TyApp t1' t2') = checkValidInstantiation t1 t1' >> checkValidInstantiation t2 t2'
    checkValidInstantiation (T.TyArr t1 t2) (T.TyArr t1' t2') = checkValidInstantiation t1 t1' >> checkValidInstantiation t2 t2'
    checkValidInstantiation (T.TyRecord fs) (T.TyRecord fs') = zipWithM_ checkValidInstantiation (HashMap.elems fs) (HashMap.elems fs')
    checkValidInstantiation TyPtr TyPtr = pass
    checkValidInstantiation t1 t2
      | t1 == t2 = pass
      | otherwise = errorOn x.value $ "Type mismatch:" <+> pretty t1 <+> "and" <+> pretty t2 <+> "are not the same"
refineExpr (Unboxed x u) = pure $ Unboxed x u
refineExpr (Apply x e1 e2) = Apply x <$> refineExpr e1 <*> refineExpr e2
refineExpr (OpApp x op e1 e2) = do
  -- Rearrange OpApp to Apply. This transformation makes code generation easier.
  let applyType = TyArr (typeOf e2) (typeOf x) -- e2 -> result
  let opType = TyArr (typeOf e1) applyType -- e1 -> e2 -> result
  let x' = Typed (typeOf x) (fst x.value)
  refineExpr $ Apply x' (Apply (x' {annotated = applyType}) (Var (x' {annotated = opType}) op) e1) e2
refineExpr (Fn x cs) = do
  cs' <- traverse refineClause cs
  env <- ask @RefineEnv
  let (patTypes, _) = splitTyArr $ typeOf x
  let typeSpaces = map (Space.normalize . Space.space env) patTypes
  let patSpaces = map (Space.normalize . Space.buildUnion) $ transpose $ NonEmpty.toList $ fmap (clauseSpace env) cs'
  exhaustive <- fmap Space.normalize <$> zipWithM Space.subtract typeSpaces patSpaces
  isEmptys <- traverse Space.equalEmpty exhaustive
  when (any not isEmptys)
    $ errorOn x.value
    $ "Pattern is not exhaustive:"
    <+> pretty exhaustive
  pure $ Fn x cs'
  where
    clauseSpace env (Clause _ ps _) = map (Space.space env) ps
refineExpr (Tuple x es) = Tuple x <$> traverse refineExpr es
refineExpr (Record x kvs) = Record x <$> traverse (\(k, v) -> (k,) <$> refineExpr v) kvs
refineExpr (Seq x ss) = Seq x <$> traverse refineStmt ss

refineClause :: (Reader RefineEnv :> es, IOE :> es, Reader Flag :> es) => Clause (Malgo Infer) -> Eff es (Clause (Malgo Refine))
refineClause (Clause x ps e) = Clause x <$> traverse refinePat ps <*> refineExpr e

refineStmt :: (Reader RefineEnv :> es, IOE :> es, Reader Flag :> es) => Stmt (Malgo Infer) -> Eff es (Stmt (Malgo Refine))
refineStmt (Let x v e) = Let x v <$> refineExpr e
refineStmt (NoBind x e) = NoBind x <$> refineExpr e

refinePat :: Pat (Malgo Infer) -> Eff es (Pat (Malgo Refine))
refinePat (VarP x v) = pure $ VarP x v
refinePat (ConP x c ps) = ConP x c <$> traverse refinePat ps
refinePat (TupleP x ps) = TupleP x <$> traverse refinePat ps
refinePat (RecordP x kps) = RecordP x <$> traverse (\(k, p) -> (k,) <$> refinePat p) kps
refinePat (UnboxedP x u) = pure $ UnboxedP x u

refineScSig :: ScSig (Malgo Infer) -> Eff es (ScSig (Malgo Refine))
refineScSig (x, name, ty) = (x,name,) <$> refineType ty

refineType :: Syn.Type (Malgo Infer) -> Eff es (Syn.Type (Malgo Refine))
refineType (Syn.TyApp x t ts) = Syn.TyApp x <$> refineType t <*> traverse refineType ts
refineType (Syn.TyVar x v) = pure $ Syn.TyVar x v
refineType (Syn.TyCon x v) = pure $ Syn.TyCon x v
refineType (Syn.TyArr x t1 t2) = Syn.TyArr x <$> refineType t1 <*> refineType t2
refineType (Syn.TyTuple x ts) = Syn.TyTuple x <$> traverse refineType ts
refineType (Syn.TyRecord x kts) = Syn.TyRecord x <$> traverse (\(k, t) -> (k,) <$> refineType t) kts

refineDataDef :: DataDef (Malgo Infer) -> Eff es (DataDef (Malgo Refine))
refineDataDef (x, name, ps, cons) = (x,name,ps,) <$> traverse (_3 $ traverse refineType) cons

refineTypeSynonym :: TypeSynonym (Malgo Infer) -> Eff es (TypeSynonym (Malgo Refine))
refineTypeSynonym (x, name, ps, typ) = (x,name,ps,) <$> refineType typ

refineForeign :: Foreign (Malgo Infer) -> Eff es (Foreign (Malgo Refine))
refineForeign (x, name, ty) = (x,name,) <$> refineType ty

refineImport :: (Applicative f) => Import (Malgo Infer) -> f (Import (Malgo Refine))
refineImport (x, modName, importList) = pure (x, modName, importList)

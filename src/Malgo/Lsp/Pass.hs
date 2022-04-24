module Malgo.Lsp.Pass where

import Control.Lens (At (at), view, (^.))
import qualified Data.HashMap.Strict as HashMap
import Koriel.Id (idName)
import Koriel.Lens
import Koriel.Pretty (Pretty (pPrint))
import Malgo.Lsp.Index
import Malgo.Prelude
import Malgo.Syntax hiding (Type)
import Malgo.Syntax.Extension
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep

index :: TcEnv -> Module (Malgo 'Refine) -> Index
index tcEnv mod = execState (runReaderT (indexModule mod) tcEnv) mempty

indexModule :: (MonadReader TcEnv m, MonadState Index m) => Module (Malgo 'Refine) -> m ()
indexModule Module {..} = indexBindGroup _moduleDefinition

indexBindGroup :: (MonadReader TcEnv m, MonadState Index m) => BindGroup (Malgo 'Refine) -> m ()
indexBindGroup BindGroup {..} = do
  traverse_ (traverse_ indexScDef) _scDefs

indexScDef :: (MonadReader TcEnv m, MonadState Index m) => ScDef (Malgo 'Refine) -> m ()
indexScDef (view value -> range, ident, expr) = do
  -- lookup the type of the variable `ident`
  identType <- lookupSignature ident
  -- index the information of this definition
  let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
  addIndex info [range]
  -- traverse the expression
  indexExp expr

indexExp :: (MonadReader TcEnv m, MonadState Index m) => Exp (Malgo 'Refine) -> m ()
indexExp (Var (view value -> range) (removePrefix -> ident)) = do
  -- lookup the type of the variable `ident`
  identType <- lookupSignature ident
  -- index the information of this definition
  let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
  addIndex info [range]
indexExp (Unboxed (view value -> range) u) = do
  let info = Info {_name = show $ pPrint u, _typeSignature = Forall [] (typeOf u), _definitions = [range]}
  addIndex info [range]
indexExp (Apply _ e1 e2) = do
  indexExp e1
  indexExp e2
indexExp (Fn _ clauses) =
  traverse_ indexClause clauses
indexExp (Tuple _ es) =
  traverse_ indexExp es
indexExp (Record _ fields) =
  traverse_ (indexExp . snd) fields
indexExp (RecordAccess _ _) = pass
indexExp (Seq _ stmts) =
  traverse_ indexStmt stmts
indexExp (Parens _ e) =
  indexExp e

indexStmt :: (MonadReader TcEnv m, MonadState Index m) => Stmt (Malgo 'Refine) -> m ()
indexStmt (Let range ident expr) = do
  identType <- lookupSignature ident
  let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
  addIndex info [range]
  indexExp expr
indexStmt (NoBind _ expr) =
  indexExp expr

indexClause :: (MonadReader TcEnv m, MonadState Index m) => Clause (Malgo 'Refine) -> m ()
indexClause (Clause _ ps e) = do
  traverse_ indexPat ps
  indexExp e

indexPat :: (MonadReader TcEnv m, MonadState Index m) => Pat (Malgo 'Refine) -> m ()
indexPat (VarP (Annotated ty range) v) = do
  -- index the information of this definition
  let info = Info {_name = v ^. idName, _typeSignature = Forall [] ty, _definitions = [range]}
  addIndex info [range]
indexPat (ConP (Annotated ty range) c ps) = do
  let info = Info {_name = c ^. idName, _typeSignature = Forall [] ty, _definitions = [range]}
  addIndex info [range]
  traverse_ indexPat ps
indexPat (TupleP _ ps) =
  traverse_ indexPat ps
indexPat (RecordP _ kps) =
  traverse_ (indexPat . snd) kps
indexPat (UnboxedP (view value -> range) u) = do
  let info = Info {_name = show $ pPrint u, _typeSignature = Forall [] (typeOf u), _definitions = [range]}
  addIndex info [range]

lookupSignature :: (MonadReader TcEnv m) => XId (Malgo 'Refine) -> m (Scheme Type)
lookupSignature ident = do
  mIdentType <- view (signatureMap . at ident)
  case mIdentType of
    Just identType -> pure identType
    Nothing -> error $ "lookupSignature: " <> show ident <> " not found"

addIndex :: (MonadState Index m) => Info -> [Range] -> m ()
addIndex info refs =
  modify $ \(Index index) -> Index $ HashMap.alter f info index
  where
    f Nothing = Just refs
    f (Just oldRefs) = Just (ordNub $ oldRefs <> refs)

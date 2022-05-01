{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Lsp.Pass where

import Control.Lens (At (at), modifying, use, view, (^.))
import Control.Lens.TH (makeFieldsNoPrefix)
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

data IndexEnv = IndexEnv
  { _signatureMap :: HashMap RnId (Scheme Type),
    _definitionMap :: HashMap RnId Info,
    _buildingIndex :: Index
  }

makeFieldsNoPrefix ''IndexEnv

newIndexEnv :: TcEnv -> IndexEnv
newIndexEnv tcEnv =
  IndexEnv
    { _signatureMap = tcEnv ^. signatureMap,
      _definitionMap = mempty,
      _buildingIndex = mempty
    }

index :: TcEnv -> Module (Malgo 'Refine) -> Index
index tcEnv mod = view buildingIndex $ execState (indexModule mod) $ newIndexEnv tcEnv

indexModule :: (MonadState IndexEnv m) => Module (Malgo 'Refine) -> m ()
indexModule Module {..} = indexBindGroup _moduleDefinition

indexBindGroup :: (MonadState IndexEnv m) => BindGroup (Malgo 'Refine) -> m ()
indexBindGroup BindGroup {..} = do
  traverse_ (traverse_ indexScDef) _scDefs

indexScDef :: (MonadState IndexEnv m) => ScDef (Malgo 'Refine) -> m ()
indexScDef (view value -> range, ident, expr) = do
  -- lookup the type of the variable `ident`
  identType <- lookupSignature ident
  -- index the information of this definition
  let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
  addIndex info [range]
  addDefinition ident info
  -- traverse the expression
  indexExp expr

indexExp :: (MonadState IndexEnv m) => Exp (Malgo 'Refine) -> m ()
indexExp (Var (view value -> range) (removePrefix -> ident)) = do
  -- lookup the infomation of this variable
  minfo <- lookupInfo ident
  case minfo of
    Nothing -> do
      identType <- lookupSignature ident
      let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
      addIndex info [range]
    Just info -> addIndex info [range]
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

indexStmt :: (MonadState IndexEnv m) => Stmt (Malgo 'Refine) -> m ()
indexStmt (Let range ident expr) = do
  identType <- lookupSignature ident
  let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
  addIndex info [range]
  addDefinition ident info
  indexExp expr
indexStmt (NoBind _ expr) =
  indexExp expr

indexClause :: (MonadState IndexEnv m) => Clause (Malgo 'Refine) -> m ()
indexClause (Clause _ ps e) = do
  traverse_ indexPat ps
  indexExp e

indexPat :: (MonadState IndexEnv m) => Pat (Malgo 'Refine) -> m ()
indexPat (VarP (Annotated ty range) v) = do
  -- index the information of this definition
  let info = Info {_name = v ^. idName, _typeSignature = Forall [] ty, _definitions = [range]}
  addIndex info [range]
  addDefinition v info
indexPat (ConP (Annotated ty range) c ps) = do
  let info = Info {_name = c ^. idName, _typeSignature = Forall [] ty, _definitions = [range]}
  addIndex info [range]
  addDefinition c info
  traverse_ indexPat ps
indexPat (TupleP _ ps) =
  traverse_ indexPat ps
indexPat (RecordP _ kps) =
  traverse_ (indexPat . snd) kps
indexPat (UnboxedP (view value -> range) u) = do
  let info = Info {_name = show $ pPrint u, _typeSignature = Forall [] (typeOf u), _definitions = [range]}
  addIndex info [range]

lookupSignature :: (MonadState IndexEnv m) => XId (Malgo 'Refine) -> m (Scheme Type)
lookupSignature ident = do
  mIdentType <- use (signatureMap . at ident)
  case mIdentType of
    Just identType -> pure identType
    Nothing -> error $ "lookupSignature: " <> show ident <> " not found"

lookupInfo :: (MonadState IndexEnv m) => XId (Malgo 'Refine) -> m (Maybe Info)
lookupInfo ident =
  use (definitionMap . at ident)

addIndex :: (MonadState IndexEnv m) => Info -> [Range] -> m ()
addIndex info refs =
  modifying buildingIndex $ \(Index index) -> Index $ HashMap.alter f info index
  where
    f Nothing = Just refs
    f (Just oldRefs) = Just (ordNub $ oldRefs <> refs)

addDefinition :: (MonadState IndexEnv m) => XId (Malgo 'Refine) -> Info -> m ()
addDefinition ident info =
  modifying definitionMap $ HashMap.insert ident info

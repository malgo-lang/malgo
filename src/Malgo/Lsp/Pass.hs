{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Malgo.Lsp.Pass where

import Control.Lens (At (at), modifying, use, view, (.~), (^.))
import Control.Lens.TH (makeFieldsNoPrefix)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Koriel.Id (Id (..), IdSort (Temporal), idName)
import Koriel.Lens
import Koriel.Pretty (Pretty (pPrint))
import Language.LSP.Types (DocumentSymbol (..), SymbolKind (..))
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Interface (HasLspIndex (lspIndex), loadInterface)
import Malgo.Lsp.Index
import Malgo.Prelude
import Malgo.Syntax hiding (Type)
import qualified Malgo.Syntax as S
import Malgo.Syntax.Extension

newtype LspOpt = LspOpt
  { _modulePaths :: [FilePath]
  }

makeFieldsNoPrefix ''LspOpt

data IndexEnv = IndexEnv
  { _signatureMap :: HashMap RnId (Scheme Type),
    _typeDefMap :: HashMap RnId (TypeDef Type),
    _buildingIndex :: Index
  }

makeFieldsNoPrefix ''IndexEnv

newIndexEnv :: TcEnv -> IndexEnv
newIndexEnv tcEnv =
  IndexEnv
    { _signatureMap = tcEnv ^. signatureMap,
      _typeDefMap = tcEnv ^. typeDefMap,
      _buildingIndex = mempty
    }

index :: (MonadIO m, MonadReader env m, HasModulePaths env [FilePath]) => TcEnv -> Module (Malgo 'Refine) -> m Index
index tcEnv mod = removeInternalInfos . view buildingIndex <$> execStateT (indexModule mod) (newIndexEnv tcEnv)

-- | Remove infos that are only used internally.
-- These infos' names start with '$'.
removeInternalInfos :: Index -> Index
removeInternalInfos (Index refs defs syms) = Index (HashMap.filterWithKey (\k _ -> not $ isInternal k) refs) defs syms
  where
    isInternal (Info {_name}) | "$" `Text.isPrefixOf` _name = True
    isInternal _ = False

indexModule :: (MonadIO m, MonadReader env m, MonadState IndexEnv m, HasModulePaths env [FilePath]) => Module (Malgo 'Refine) -> m ()
indexModule Module {..} = indexBindGroup _moduleDefinition

indexBindGroup :: (MonadIO m, MonadReader env m, MonadState IndexEnv m, HasModulePaths env [FilePath]) => BindGroup (Malgo 'Refine) -> m ()
indexBindGroup BindGroup {..} = do
  traverse_ indexImport _imports
  traverse_ indexDataDef _dataDefs
  traverse_ indexScSig _scSigs
  traverse_ (traverse_ indexScDef) _scDefs

indexImport :: (MonadIO m, MonadReader env m, MonadState IndexEnv m, HasModulePaths env [FilePath]) => Import (Malgo 'Refine) -> m ()
indexImport (_, moduleName, _) = do
  -- include the index file of the imported module
  minterface <- loadInterface moduleName
  case minterface of
    Nothing ->
      error $ "Could not find interface file for module " <> show moduleName
    Just interface -> do
      -- Merge imported module's interface without document symbol infomations
      let index = interface ^. lspIndex & symbolInfo .~ mempty
      modifying buildingIndex (`mappend` index)

indexDataDef :: MonadState IndexEnv m => DataDef (Malgo 'Refine) -> m ()
indexDataDef (range, typeName, typeParameters, constructors) = do
  typeKind <- lookupTypeKind typeName
  let info = Info {_name = typeName ^. idName, _typeSignature = Forall [] typeKind, _definitions = [range]}
  addReferences info [range]
  addDefinition typeName info
  addSymbolInfo typeName (symbol SkEnum typeName range)

  for_ typeParameters \(range, typeParameter) -> do
    typeParameterKind <- lookupTypeKind typeParameter
    let info = Info {_name = typeParameter ^. idName, _typeSignature = Forall [] typeParameterKind, _definitions = [range]}
    addReferences info [range]
    addDefinition typeParameter info
    addSymbolInfo typeParameter (symbol SkTypeParameter typeParameter range)

  traverse_ indexConstructor constructors
  where
    indexConstructor (range, constructor, parameters) = do
      constructorType <- lookupSignature constructor
      let info = Info {_name = constructor ^. idName, _typeSignature = constructorType, _definitions = [range]}
      addReferences info [range]
      addDefinition constructor info
      addSymbolInfo constructor (symbol SkEnumMember constructor range)
      traverse_ indexType parameters

indexType :: MonadState IndexEnv m => S.Type (Malgo 'Refine) -> m ()
indexType (S.TyApp _ t ts) = do
  indexType t
  traverse_ indexType ts
indexType (S.TyVar range name) = do
  minfo <- lookupInfo name
  case minfo of
    Nothing -> pass -- TODO: add new entry
    Just info -> addReferences info [range]
indexType (S.TyCon range name) = do
  minfo <- lookupInfo name
  case minfo of
    Nothing -> pass -- TODO: add new entry
    Just info -> addReferences info [range]
indexType (S.TyArr _ t1 t2) = do
  indexType t1
  indexType t2
indexType (S.TyTuple _ ts) = traverse_ indexType ts
indexType (S.TyRecord _ fields) = traverse_ (indexType . snd) fields

indexScSig :: MonadState IndexEnv m => ScSig (Malgo 'Refine) -> m ()
indexScSig (range, ident, _) = do
  -- lookup the infomation of this variable
  minfo <- lookupInfo ident
  case minfo of
    Nothing -> do
      identType <- lookupSignature ident
      let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
      addReferences info [range]
    Just info -> addReferences info [range]

indexScDef :: (MonadState IndexEnv m) => ScDef (Malgo 'Refine) -> m ()
indexScDef (view value -> range, ident, expr) = do
  minfo <- lookupInfo ident
  case minfo of
    Nothing -> do
      -- lookup the type of the variable `ident`
      identType <- lookupSignature ident
      -- index the information of this definition
      let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
      addReferences info [range]
      addDefinition ident info
      addSymbolInfo ident (symbol SkFunction ident range)
    Just info -> do
      addReferences info [range]
      addDefinition ident info
      addSymbolInfo ident (symbol SkFunction ident range)
  -- traverse the expression
  indexExp expr

indexExp :: (MonadState IndexEnv m) => Exp (Malgo 'Refine) -> m ()
indexExp (Var (view value -> range) ident) = do
  -- lookup the infomation of this variable
  minfo <- lookupInfo ident
  case minfo of
    Nothing -> do
      identType <- lookupSignature ident
      let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = []}
      addReferences info [range]
    Just info -> addReferences info [range]
indexExp (Unboxed (view value -> range) u) = do
  let info = Info {_name = show $ pPrint u, _typeSignature = Forall [] (typeOf u), _definitions = [range]}
  addReferences info [range]
indexExp (Apply _ e1 e2) = do
  indexExp e1
  indexExp e2
indexExp (Fn _ clauses) =
  traverse_ indexClause clauses
indexExp (Tuple _ es) =
  traverse_ indexExp es
indexExp (Record _ fields) =
  traverse_ (indexExp . snd) fields
indexExp (Seq _ stmts) =
  traverse_ indexStmt stmts
indexExp (Parens _ e) =
  indexExp e

indexStmt :: (MonadState IndexEnv m) => Stmt (Malgo 'Refine) -> m ()
indexStmt (Let _ (Id _ _ _ Temporal) expr) = indexExp expr
indexStmt (Let range ident expr) = do
  identType <- lookupSignature ident
  let info = Info {_name = ident ^. idName, _typeSignature = identType, _definitions = [range]}
  addReferences info [range]
  addDefinition ident info
  addSymbolInfo ident (symbol SkVariable ident range)
  indexExp expr
indexStmt (NoBind _ expr) =
  indexExp expr

indexClause :: (MonadState IndexEnv m) => Clause (Malgo 'Refine) -> m ()
indexClause (Clause _ ps e) = do
  traverse_ indexPat ps
  indexExp e

indexPat :: (MonadState IndexEnv m) => Pat (Malgo 'Refine) -> m ()
indexPat (VarP _ (Id _ _ _ Temporal)) = pass
indexPat (VarP (Typed ty range) v) = do
  -- index the information of this definition
  let info = Info {_name = v ^. idName, _typeSignature = Forall [] ty, _definitions = [range]}
  addReferences info [range]
  addDefinition v info
  addSymbolInfo v (symbol SkVariable v range)
indexPat (ConP (Typed _ range) c ps) = do
  minfo <- lookupInfo c
  case minfo of
    Nothing -> pass
    Just info -> addReferences info [range]
  traverse_ indexPat ps
indexPat (TupleP _ ps) =
  traverse_ indexPat ps
indexPat (RecordP _ kps) =
  traverse_ (indexPat . snd) kps
indexPat (UnboxedP (view value -> range) u) = do
  let info = Info {_name = show $ pPrint u, _typeSignature = Forall [] (typeOf u), _definitions = [range]}
  addReferences info [range]

lookupSignature :: (MonadState IndexEnv m) => XId (Malgo 'Refine) -> m (Scheme Type)
lookupSignature ident = do
  mIdentType <- use (signatureMap . at ident)
  case mIdentType of
    Just identType -> pure identType
    Nothing -> error $ "lookupSignature: " <> show ident <> " not found"

lookupTypeKind :: MonadState IndexEnv m => XId (Malgo 'Refine) -> m Kind
lookupTypeKind typeName = do
  mTypeDef <- use (typeDefMap . at typeName)
  case mTypeDef of
    Just typeDef -> pure $ kindOf (typeDef ^. typeConstructor)
    Nothing -> error $ "lookupTypeKind: " <> show typeName <> " not found"

lookupInfo :: (MonadState IndexEnv m) => XId (Malgo 'Refine) -> m (Maybe Info)
lookupInfo ident =
  use (buildingIndex . definitionMap . at ident)

addReferences :: (MonadState IndexEnv m) => Info -> [Range] -> m ()
addReferences info refs =
  modifying buildingIndex $ \(Index refs defs syms) -> Index (HashMap.alter f info refs) defs syms
  where
    f Nothing = Just refs
    f (Just oldRefs) = Just (ordNub $ oldRefs <> refs)

addDefinition :: (MonadState IndexEnv m) => XId (Malgo 'Refine) -> Info -> m ()
addDefinition ident info =
  modifying (buildingIndex . definitionMap) $ HashMap.insert ident info

addSymbolInfo :: (MonadState IndexEnv m) => XId (Malgo 'Refine) -> DocumentSymbol -> m ()
addSymbolInfo ident symbol =
  modifying (buildingIndex . symbolInfo) $ HashMap.insert ident symbol

symbol :: SymbolKind -> Id a -> Range -> DocumentSymbol
symbol kind name range =
  DocumentSymbol
    { _name = name ^. idName,
      _detail = Nothing,
      _kind = kind,
      _tags = Nothing,
      _deprecated = Nothing,
      _range = malgoRangeToLspRange range,
      _selectionRange = malgoRangeToLspRange range,
      _children = Nothing
    }

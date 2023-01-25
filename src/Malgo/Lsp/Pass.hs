{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Malgo.Lsp.Pass (index) where

import Control.Lens ((^.))
import Control.Lens.TH (makeFieldsNoPrefix)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Koriel.Id (Id (..), IdSort (Temporal), name)
import Koriel.Pretty (Pretty (pPrint))
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Lsp.Index
import Malgo.Monad (MalgoEnv, MalgoM)
import Malgo.Prelude
import Malgo.Syntax hiding (Type)
import Malgo.Syntax qualified as S
import Malgo.Syntax.Extension

data IndexEnv = IndexEnv
  { signatureMap :: HashMap RnId (Scheme Type),
    typeDefMap :: HashMap RnId (TypeDef Type),
    kindCtx :: KindCtx,
    buildingIndex :: Index
  }

makeFieldsNoPrefix ''IndexEnv

newIndexEnv :: TcEnv -> IndexEnv
newIndexEnv tcEnv =
  IndexEnv
    { signatureMap = tcEnv._signatureMap,
      typeDefMap = tcEnv._typeDefMap,
      kindCtx = tcEnv._kindCtx,
      buildingIndex = mempty
    }

index :: TcEnv -> Module (Malgo 'Refine) -> MalgoM Index
index tcEnv mod = do
  removeInternalInfos . asks (.buildingIndex) <$> execStateT (indexModule mod) (newIndexEnv tcEnv)

-- | Remove infos that are only used internally.
-- These infos' names start with '$'.
removeInternalInfos :: Index -> Index
removeInternalInfos (Index refs defs syms) = Index (HashMap.filterWithKey (\k _ -> not $ isInternal k) refs) defs syms
  where
    isInternal (Info {name}) | "$" `Text.isPrefixOf` name = True
    isInternal _ = False

indexModule :: (MonadIO m, MonadReader MalgoEnv m, MonadState IndexEnv m) => Module (Malgo 'Refine) -> m ()
indexModule Module {..} = indexBindGroup _moduleDefinition

indexBindGroup :: (MonadIO m, MonadReader MalgoEnv m, MonadState IndexEnv m) => BindGroup (Malgo 'Refine) -> m ()
indexBindGroup BindGroup {..} = do
  traverse_ indexImport _imports
  traverse_ indexDataDef _dataDefs
  traverse_ indexScSig _scSigs
  traverse_ (traverse_ indexScDef) _scDefs

indexImport :: (MonadIO m, MonadReader MalgoEnv m, MonadState IndexEnv m) => Import (Malgo 'Refine) -> m ()
indexImport (_, moduleName, _) = do
  -- include the index file of the imported module
  mindex <- loadIndex moduleName
  case mindex of
    Nothing ->
      error $ "Could not find index file for module " <> show moduleName
    Just index -> do
      -- Merge imported module's interface without document symbol infomations
      index <- pure $ index {symbolInfo = mempty}
      modify \x -> x {buildingIndex = x.buildingIndex `mappend` index}

indexDataDef :: MonadState IndexEnv m => DataDef (Malgo 'Refine) -> m ()
indexDataDef (range, typeName, typeParameters, constructors) = do
  typeKind <- lookupTypeKind typeName
  let info = Info {name = typeName.name, typeSignature = Forall [] typeKind, definitions = [range]}
  addReferences info [range]
  addDefinition typeName info
  addSymbolInfo typeName (symbol Data typeName range)

  for_ typeParameters \(range, typeParameter) -> do
    typeParameterKind <- lookupTypeKind typeParameter
    let info = Info {name = typeParameter.name, typeSignature = Forall [] typeParameterKind, definitions = [range]}
    addReferences info [range]
    addDefinition typeParameter info
    addSymbolInfo typeParameter (symbol TypeParam typeParameter range)

  traverse_ indexConstructor constructors
  where
    indexConstructor (range, constructor, parameters) = do
      constructorType <- lookupSignature constructor
      let info = Info {name = constructor.name, typeSignature = constructorType, definitions = [range]}
      addReferences info [range]
      addDefinition constructor info
      addSymbolInfo constructor (symbol Constructor constructor range)
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
      let info = Info {name = ident.name, typeSignature = identType, definitions = [range]}
      addReferences info [range]
    Just info -> addReferences info [range]

indexScDef :: MonadState IndexEnv m => ScDef (Malgo 'Refine) -> m ()
indexScDef (Typed {value = range}, ident, expr) = do
  minfo <- lookupInfo ident
  case minfo of
    Nothing -> do
      -- lookup the type of the variable `ident`
      identType <- lookupSignature ident
      -- index the information of this definition
      let info = Info {name = ident.name, typeSignature = identType, definitions = [range]}
      addReferences info [range]
      addDefinition ident info
      addSymbolInfo ident (symbol Function ident range)
    Just info -> do
      addReferences info [range]
      addDefinition ident info
      addSymbolInfo ident (symbol Function ident range)
  -- traverse the expression
  indexExp expr

indexExp :: MonadState IndexEnv m => Exp (Malgo 'Refine) -> m ()
indexExp (Var Typed {value = range} ident) = do
  -- lookup the infomation of this variable
  minfo <- lookupInfo ident
  case minfo of
    Nothing -> do
      identType <- lookupSignature ident
      let info = Info {name = ident.name, typeSignature = identType, definitions = []}
      addReferences info [range]
    Just info -> addReferences info [range]
indexExp (Unboxed Typed {value = range} u) = do
  let info = Info {name = show $ pPrint u, typeSignature = Forall [] (typeOf u), definitions = [range]}
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

indexStmt :: MonadState IndexEnv m => Stmt (Malgo 'Refine) -> m ()
indexStmt (Let _ Id {sort = Temporal} expr) = indexExp expr
indexStmt (Let range ident expr) = do
  identType <- lookupSignature ident
  let info = Info {name = ident.name, typeSignature = identType, definitions = [range]}
  addReferences info [range]
  addDefinition ident info
  addSymbolInfo ident (symbol Variable ident range)
  indexExp expr
indexStmt (NoBind _ expr) =
  indexExp expr

indexClause :: MonadState IndexEnv m => Clause (Malgo 'Refine) -> m ()
indexClause (Clause _ ps e) = do
  traverse_ indexPat ps
  indexExp e

indexPat :: MonadState IndexEnv m => Pat (Malgo 'Refine) -> m ()
indexPat (VarP _ Id {sort = Temporal}) = pass
indexPat (VarP (Typed ty range) v) = do
  -- index the information of this definition
  let info = Info {name = v.name, typeSignature = Forall [] ty, definitions = [range]}
  addReferences info [range]
  addDefinition v info
  addSymbolInfo v (symbol Variable v range)
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
indexPat (UnboxedP Typed {value = range} u) = do
  let info = Info {name = show $ pPrint u, typeSignature = Forall [] (typeOf u), definitions = [range]}
  addReferences info [range]

lookupSignature :: MonadState IndexEnv m => XId (Malgo 'Refine) -> m (Scheme Type)
lookupSignature ident = do
  mIdentType <- gets (HashMap.lookup ident . (.signatureMap))
  case mIdentType of
    Just identType -> pure identType
    Nothing -> error $ "lookupSignature: " <> show ident <> " not found"

lookupTypeKind :: MonadState IndexEnv m => XId (Malgo 'Refine) -> m Kind
lookupTypeKind typeName = do
  mTypeDef <- gets (HashMap.lookup typeName . (.typeDefMap))
  case mTypeDef of
    Just typeDef -> do
      ctx <- gets (.kindCtx)
      pure $ kindOf ctx (typeDef ^. typeConstructor)
    Nothing -> error $ "lookupTypeKind: " <> show typeName <> " not found"

lookupInfo :: MonadState IndexEnv m => XId (Malgo 'Refine) -> m (Maybe Info)
lookupInfo ident =
  gets (HashMap.lookup ident . (.definitionMap) . (.buildingIndex))

addReferences :: MonadState IndexEnv m => Info -> [Range] -> m ()
addReferences info refs =
  modify \x ->
    x
      { buildingIndex =
          let Index {..} = x.buildingIndex
           in Index
                { references =
                    HashMap.insert
                      info
                      (refs <> HashMap.lookupDefault [] info references)
                      references,
                  definitionMap,
                  symbolInfo
                }
      }

addDefinition :: MonadState IndexEnv m => XId (Malgo 'Refine) -> Info -> m ()
addDefinition ident info =
  modify \x ->
    x
      { buildingIndex =
          let Index {..} = x.buildingIndex
           in Index
                { references,
                  definitionMap = HashMap.insert ident info definitionMap,
                  symbolInfo
                }
      }

addSymbolInfo :: MonadState IndexEnv m => XId (Malgo 'Refine) -> Symbol -> m ()
addSymbolInfo ident symbol =
  modify \x ->
    x
      { buildingIndex =
          let Index {..} = x.buildingIndex
           in Index
                { references,
                  definitionMap,
                  symbolInfo = HashMap.insert ident symbol symbolInfo
                }
      }

symbol :: SymbolKind -> Id a -> Range -> Symbol
symbol kind name = Symbol kind name.name

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Malgo.Lsp.Pass (index) where

import Control.Lens (view, (.~), (^.))
import Control.Lens.TH (makeFieldsNoPrefix)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Koriel.Id (Id (..), IdSort (Temporal), ModuleName, name)
import Koriel.Lens
import Koriel.Pretty (Pretty (pretty))
import Malgo.Infer.TcEnv
import Malgo.Infer.TypeRep
import Malgo.Interface
import Malgo.Lsp.Index
import Malgo.Prelude
import Malgo.Syntax hiding (Type)
import Malgo.Syntax qualified as S
import Malgo.Syntax.Extension

data IndexEnv = IndexEnv
  { _signatureMap :: HashMap RnId (Scheme Type),
    _typeDefMap :: HashMap RnId (TypeDef Type),
    _kindCtx :: KindCtx,
    _buildingIndex :: Index
  }

makeFieldsNoPrefix ''IndexEnv

newIndexEnv :: TcEnv -> IndexEnv
newIndexEnv tcEnv =
  IndexEnv
    { _signatureMap = tcEnv ^. signatureMap,
      _typeDefMap = tcEnv ^. typeDefMap,
      _kindCtx = tcEnv ^. kindCtx,
      _buildingIndex = mempty
    }

index :: (Reader ModulePathList :> es, State (HashMap ModuleName Index) :> es, IOE :> es) => TcEnv -> Module (Malgo 'Refine) -> Eff es Index
index tcEnv mod = do
  removeInternalInfos . view buildingIndex <$> execState (newIndexEnv tcEnv) (indexModule mod)

-- | Remove infos that are only used internally.
-- These infos' names start with '$'.
removeInternalInfos :: Index -> Index
removeInternalInfos (Index refs defs syms) = Index (HashMap.filterWithKey (\k _ -> not $ isInternal k) refs) defs syms
  where
    isInternal (Info {name}) | "$" `T.isPrefixOf` name = True
    isInternal _ = False

indexModule :: (State IndexEnv :> es, Reader ModulePathList :> es, State (HashMap ModuleName Index) :> es, IOE :> es) => Module (Malgo 'Refine) -> Eff es ()
indexModule Module {..} = indexBindGroup moduleDefinition

indexBindGroup ::
  ( Reader ModulePathList :> es,
    State (HashMap ModuleName Index) :> es,
    State IndexEnv :> es,
    IOE :> es
  ) =>
  BindGroup (Malgo 'Refine) ->
  Eff es ()
indexBindGroup BindGroup {..} = do
  traverse_ indexImport _imports
  traverse_ indexDataDef _dataDefs
  traverse_ indexScSig _scSigs
  traverse_ (traverse_ indexScDef) _scDefs

indexImport ::
  ( Reader ModulePathList :> es,
    State (HashMap ModuleName Index) :> es,
    State IndexEnv :> es,
    IOE :> es
  ) =>
  Import (Malgo 'Refine) ->
  Eff es ()
indexImport (_, moduleName, _) = do
  -- include the index file of the imported module
  mindex <- loadIndex moduleName
  case mindex of
    Nothing ->
      error $ "Could not find index file for module " <> show moduleName
    Just index -> do
      -- Merge imported module's interface without document symbol infomations
      index <- pure $ index & symbolInfo .~ mempty
      modify \s@IndexEnv {..} -> s {_buildingIndex = _buildingIndex <> index}

indexDataDef :: (State IndexEnv :> es) => DataDef (Malgo 'Refine) -> Eff es ()
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

indexType :: (State IndexEnv :> es) => S.Type (Malgo 'Refine) -> Eff es ()
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

indexScSig :: (State IndexEnv :> es) => ScSig (Malgo Refine) -> Eff es ()
indexScSig (range, ident, _) = do
  -- lookup the infomation of this variable
  minfo <- lookupInfo ident
  case minfo of
    Nothing -> do
      identType <- lookupSignature ident
      let info = Info {name = ident.name, typeSignature = identType, definitions = [range]}
      addReferences info [range]
    Just info -> addReferences info [range]

indexScDef :: (State IndexEnv :> es) => ScDef (Malgo Refine) -> Eff es ()
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
  indexExpr expr

indexExpr :: (State IndexEnv :> es) => Expr (Malgo Refine) -> Eff es ()
indexExpr (Var Typed {value = range} ident) = do
  -- lookup the infomation of this variable
  minfo <- lookupInfo ident
  case minfo of
    Nothing -> do
      identType <- lookupSignature ident
      let info = Info {name = ident.name, typeSignature = identType, definitions = []}
      addReferences info [range]
    Just info -> addReferences info [range]
indexExpr (Unboxed Typed {value = range} u) = do
  let info = Info {name = convertString $ show $ pretty u, typeSignature = Forall [] (typeOf u), definitions = [range]}
  addReferences info [range]
indexExpr (Apply _ e1 e2) = do
  indexExpr e1
  indexExpr e2
indexExpr (Fn _ clauses) =
  traverse_ indexClause clauses
indexExpr (Tuple _ es) =
  traverse_ indexExpr es
indexExpr (Record _ fields) =
  traverse_ (indexExpr . snd) fields
indexExpr (Seq _ stmts) =
  traverse_ indexStmt stmts

indexStmt :: (State IndexEnv :> es) => Stmt (Malgo Refine) -> Eff es ()
indexStmt (Let _ Id {sort = Temporal _} expr) = indexExpr expr
indexStmt (Let range ident expr) = do
  identType <- lookupSignature ident
  let info = Info {name = ident.name, typeSignature = identType, definitions = [range]}
  addReferences info [range]
  addDefinition ident info
  addSymbolInfo ident (symbol Variable ident range)
  indexExpr expr
indexStmt (NoBind _ expr) =
  indexExpr expr

indexClause :: (State IndexEnv :> es) => Clause (Malgo Refine) -> Eff es ()
indexClause (Clause _ ps e) = do
  traverse_ indexPat ps
  indexExpr e

indexPat :: (State IndexEnv :> es) => Pat (Malgo 'Refine) -> Eff es ()
indexPat (VarP _ Id {sort = Temporal _}) = pass
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
  let info = Info {name = convertString $ show $ pretty u, typeSignature = Forall [] (typeOf u), definitions = [range]}
  addReferences info [range]

lookupSignature :: (State IndexEnv :> es) => Id () -> Eff es (Scheme Type)
lookupSignature ident = do
  mIdentType <- gets @IndexEnv ((._signatureMap) >>> HashMap.lookup ident)
  case mIdentType of
    Just identType -> pure identType
    Nothing -> error $ "lookupSignature: " <> show ident <> " not found"

lookupTypeKind :: (State IndexEnv :> es) => Id () -> Eff es Kind
lookupTypeKind typeName = do
  mTypeDef <- gets @IndexEnv ((._typeDefMap) >>> HashMap.lookup typeName)
  case mTypeDef of
    Just typeDef -> do
      ctx <- gets @IndexEnv (._kindCtx)
      pure $ kindOf ctx (typeDef ^. typeConstructor)
    Nothing -> error $ "lookupTypeKind: " <> show typeName <> " not found"

lookupInfo :: (State IndexEnv :> es) => Id () -> Eff es (Maybe Info)
lookupInfo ident =
  gets @IndexEnv ((._buildingIndex) >>> (._definitionMap) >>> HashMap.lookup ident)

addReferences :: (State IndexEnv :> es) => Info -> [Range] -> Eff es ()
addReferences info refs =
  modify \s@IndexEnv {..} ->
    s
      { _buildingIndex =
          _buildingIndex
            { references = HashMap.insert info (refs <> HashMap.lookupDefault [] info _buildingIndex.references) _buildingIndex.references
            }
      }

addDefinition :: (State IndexEnv :> es) => Id () -> Info -> Eff es ()
addDefinition ident info =
  modify \s@IndexEnv {..} ->
    s
      { _buildingIndex =
          _buildingIndex
            { _definitionMap = HashMap.insert ident info _buildingIndex._definitionMap
            }
      }

addSymbolInfo :: (State IndexEnv :> es) => Id () -> Symbol -> Eff es ()
addSymbolInfo ident symbol =
  modify \s@IndexEnv {..} ->
    s
      { _buildingIndex =
          _buildingIndex
            { _symbolInfo = HashMap.insert ident symbol _buildingIndex._symbolInfo
            }
      }

symbol :: SymbolKind -> Id a -> Range -> Symbol
symbol kind name = Symbol kind name.name

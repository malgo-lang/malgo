{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Interface
  ( Interface (..),
    buildInterface,
    loadInterface,
    externalFromInterface,
    exportedIdentList,
    exportedTypeIdentList,
  )
where

import Control.Lens (ifor_)
import Data.Map.Strict qualified as Map
import Data.Store (Store)
import Effectful (Eff, IOE, runPureEff, (:>))
import Effectful.State.Static.Local (State, execState, get, modify)
import GHC.Records (HasField)
import Malgo.Id
import Malgo.Infer.TypeRep (KindCtx, insertKind)
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Module
import Malgo.Prelude
import Malgo.Syntax.Extension
import Prettyprinter (viaShow)

data Interface = Interface
  { moduleName :: ModuleName,
    -- | Used in Infer
    signatureMap :: Map PsId (GT.Scheme GT.Type),
    -- | Used in Infer
    typeDefMap :: Map PsId (GT.TypeDef GT.Type),
    -- | Used in Infer
    typeSynonymMap :: Map GT.TypeVar ([GT.TypeVar], GT.Type),
    kindCtx :: KindCtx,
    -- | Used in Rename
    infixInfo :: Map PsId (Assoc, Int),
    -- | Used in Rename
    dependencies :: Set ModuleName
  }
  deriving stock (Show, Generic)

instance Store Interface

instance Pretty Interface where
  pretty = viaShow

externalFromInterface :: Interface -> PsId -> RnId
externalFromInterface Interface {moduleName} psId =
  Id {name = psId, sort = External, moduleName}

buildInterface ::
  ( HasField "typeSynonymMap" tcEnv (Map GT.TypeVar ([GT.TypeVar], GT.Type)),
    HasField "signatureMap" tcEnv (Map RnId (GT.Scheme GT.Type)),
    HasField "typeDefMap" tcEnv (Map RnId (GT.TypeDef GT.Type)),
    HasField "kindCtx" tcEnv KindCtx,
    HasField "dependencies" rnState (Set ModuleName),
    HasField "infixInfo" rnState (Map Id (Assoc, Int))
  ) =>
  ModuleName ->
  rnState ->
  tcEnv ->
  Interface
buildInterface moduleName rnState tcEnv =
  let inf =
        Interface
          { moduleName,
            signatureMap = mempty,
            typeDefMap = mempty,
            typeSynonymMap = mempty,
            kindCtx = mempty,
            infixInfo = Map.mapKeys (\id -> id.name) rnState.infixInfo,
            dependencies = rnState.dependencies
          }
   in runPureEff $ execState inf do
        ifor_ tcEnv.signatureMap $ \tcId scheme ->
          when (tcId.sort == External && tcId.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {signatureMap = Map.insert tcId.name scheme signatureMap}
        ifor_ tcEnv.typeDefMap $ \rnId typeDef -> do
          when (rnId.sort == External && rnId.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {typeDefMap = Map.insert rnId.name typeDef typeDefMap}
        ifor_ tcEnv.typeSynonymMap $ \tv (tvs, ty) -> do
          when (tv.sort == External && tv.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {typeSynonymMap = Map.insert tv (tvs, ty) typeSynonymMap}
        ifor_ tcEnv.kindCtx $ \tv kind -> do
          when (tv.sort == External && tv.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {kindCtx = insertKind tv kind kindCtx}

exportedIdentList :: (HasField "signatureMap" r (Map k v)) => r -> [k]
exportedIdentList inf = Map.keys inf.signatureMap

exportedTypeIdentList ::
  ( HasField "typeDefMap" inf (Map name a),
    HasField "typeSynonymMap" inf (Map id b),
    HasField "name" id name
  ) =>
  inf ->
  [name]
exportedTypeIdentList inf = Map.keys inf.typeDefMap <> map (\id -> id.name) (Map.keys inf.typeSynonymMap)

loadInterface ::
  (HasCallStack) =>
  (IOE :> es, Workspace :> es, State (Map ModuleName Interface) :> es) =>
  ModuleName ->
  Eff es Interface
loadInterface modName = do
  interfaces <- get
  case Map.lookup modName interfaces of
    Just interface -> pure interface
    Nothing -> do
      modPath <- getModulePath modName
      ViaStore interface <- load modPath ".mlgi"
      modify $ Map.insert modName interface
      pure interface
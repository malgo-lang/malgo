{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Malgo.Interface
  ( Interface (..),
    buildInterface,
    toInterfacePath,
    loadInterface,
    externalFromInterface,
    exportedIdentList,
    exportedTypeIdentList,
  )
where

import Control.Lens (ifor_, (^.))
import Control.Lens.TH
import Data.HashMap.Strict qualified as HashMap
import Data.Store (Store)
import Effectful (Eff, IOE, runPureEff, (:>))
import Effectful.State.Static.Local (State, execState, get, modify)
import GHC.Records (HasField)
import Malgo.Core.Type qualified as C
import Malgo.Desugar.DsState (DsState, HasNameEnv (nameEnv))
import Malgo.Id
import Malgo.Infer.TypeRep (KindCtx, insertKind)
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Lens
import Malgo.Module
import Malgo.Prelude
import Malgo.Rename.RnState (RnState)
import Malgo.Rename.RnState qualified as RnState
import Malgo.Syntax.Extension
import System.FilePath (replaceExtension)

data Interface = Interface
  { moduleName :: ModuleName,
    -- | Used in Infer
    signatureMap :: HashMap PsId (GT.Scheme GT.Type),
    -- | Used in Infer
    typeDefMap :: HashMap PsId (GT.TypeDef GT.Type),
    -- | Used in Infer
    typeSynonymMap :: HashMap GT.TypeVar ([GT.TypeVar], GT.Type),
    kindCtx :: KindCtx,
    -- | Used in Desugar
    coreIdentMap :: HashMap PsId (Meta C.Type),
    -- | Used in Rename
    infixInfo :: HashMap PsId (Assoc, Int),
    -- | Used in Rename
    dependencies :: HashSet ModuleName
  }
  deriving stock (Show, Generic)

instance Store Interface

makeFieldsNoPrefix ''Interface

instance Pretty Interface where
  pretty = viaShow

externalFromInterface :: Interface -> PsId -> RnId
externalFromInterface Interface {moduleName} psId =
  Id {name = psId, sort = External, moduleName}

buildInterface ::
  (HasTypeSynonymMap tcEnv (HashMap GT.TypeVar ([GT.TypeVar], GT.Type))) =>
  ModuleName ->
  RnState ->
  tcEnv ->
  DsState ->
  Interface
-- TODO: write abbrMap to interface
buildInterface moduleName rnState tcEnv dsState =
  let inf =
        Interface
          { moduleName,
            signatureMap = mempty,
            typeDefMap = mempty,
            typeSynonymMap = mempty,
            kindCtx = mempty,
            coreIdentMap = mempty,
            infixInfo = HashMap.mapKeys (\id -> id.name) rnState.infixInfo,
            dependencies = rnState.dependencies
          }
   in runPureEff $ execState inf do
        ifor_ (dsState ^. nameEnv) $ \tcId coreId ->
          when (tcId.sort == External && tcId.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {coreIdentMap = HashMap.insert tcId.name coreId coreIdentMap}
        ifor_ (dsState ^. signatureMap) $ \tcId scheme ->
          when (tcId.sort == External && tcId.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {signatureMap = HashMap.insert tcId.name scheme signatureMap}
        ifor_ (dsState ^. typeDefMap) $ \rnId typeDef -> do
          when (rnId.sort == External && rnId.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {typeDefMap = HashMap.insert rnId.name typeDef typeDefMap}
        ifor_ (tcEnv ^. typeSynonymMap) $ \tv (tvs, ty) -> do
          when (tv.sort == External && tv.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {typeSynonymMap = HashMap.insert tv (tvs, ty) typeSynonymMap}
        ifor_ (dsState ^. kindCtx) $ \tv kind -> do
          when (tv.sort == External && tv.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {kindCtx = insertKind tv kind kindCtx}

exportedIdentList :: (HasField "signatureMap" r (HashMap k v)) => r -> [k]
exportedIdentList inf = HashMap.keys inf.signatureMap

exportedTypeIdentList ::
  ( HasField "typeDefMap" inf (HashMap name a),
    HasField "typeSynonymMap" inf (HashMap id b),
    HasField "name" id name
  ) =>
  inf ->
  [name]
exportedTypeIdentList inf = HashMap.keys inf.typeDefMap <> map (\id -> id.name) (HashMap.keys inf.typeSynonymMap)

toInterfacePath :: String -> FilePath
toInterfacePath x = replaceExtension x "mlgi"

loadInterface ::
  (HasCallStack) =>
  (IOE :> es, Workspace :> es, State (HashMap ModuleName Interface) :> es) =>
  ModuleName ->
  Eff es Interface
loadInterface modName = do
  interfaces <- get
  case HashMap.lookup modName interfaces of
    Just interface -> pure interface
    Nothing -> do
      modPath <- getModulePath modName
      ViaStore interface <- load modPath ".mlgi"
      modify $ HashMap.insert modName interface
      pure interface
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Interface (Interface (..), ModulePathList (..), coreIdentMap, buildInterface, toInterfacePath, loadInterface, externalFromInterface) where

import Control.Exception (IOException, catch)
import Control.Lens (ifor_, (^.))
import Control.Lens.TH
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.Store (Store, decodeEx)
import Effectful (Eff, IOE, runPureEff, (:>))
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Local (State, execState, get, modify)
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.Lens
import Koriel.Pretty
import Malgo.Desugar.DsState (DsState, HasNameEnv (nameEnv))
import Malgo.Infer.TypeRep (KindCtx, insertKind)
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Prelude
import Malgo.Rename.RnState (RnState)
import Malgo.Rename.RnState qualified as RnState
import Malgo.Syntax.Extension
import System.Directory qualified as Directory
import System.FilePath (replaceExtension, (</>))

newtype ModulePathList = ModulePathList [FilePath]

data Interface = Interface
  { moduleName :: ModuleName,
    -- | Used in Infer
    signatureMap :: HashMap PsId (GT.Scheme GT.Type),
    -- | Used in Infer
    typeDefMap :: HashMap PsId (GT.TypeDef GT.Type),
    -- | Used in Infer
    _typeSynonymMap :: HashMap GT.TypeVar ([GT.TypeVar], GT.Type),
    _kindCtx :: KindCtx,
    -- | Used in Rename
    exportedIdentList :: [PsId],
    -- | Used in Rename
    exportedTypeIdentList :: [PsId],
    -- | Used in Desugar
    _coreIdentMap :: HashMap RnId (Id C.Type),
    -- | Used in Rename
    infixInfo :: HashMap RnId (Assoc, Int),
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
  Id {name = psId, sort = External, meta = (), moduleName}

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
            _typeSynonymMap = mempty,
            _kindCtx = mempty,
            exportedIdentList = mempty,
            exportedTypeIdentList = mempty,
            _coreIdentMap = mempty,
            infixInfo = rnState.infixInfo,
            dependencies = rnState.dependencies
          }
   in runPureEff $ execState inf do
        ifor_ (dsState ^. nameEnv) $ \tcId coreId ->
          when (tcId.sort == External && tcId.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf
                { exportedIdentList = tcId.name : exportedIdentList,
                  _coreIdentMap = HashMap.insert tcId coreId _coreIdentMap
                }
        ifor_ (dsState ^. signatureMap) $ \tcId scheme ->
          when (tcId.sort == External && tcId.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {signatureMap = HashMap.insert tcId.name scheme signatureMap}
        ifor_ (dsState ^. typeDefMap) $ \rnId typeDef -> do
          when (rnId.sort == External && rnId.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf
                { exportedTypeIdentList = rnId.name : exportedTypeIdentList,
                  typeDefMap = HashMap.insert rnId.name typeDef typeDefMap
                }
        ifor_ (tcEnv ^. typeSynonymMap) $ \tv (tvs, ty) -> do
          when (tv.sort == External && tv.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {_typeSynonymMap = HashMap.insert tv (tvs, ty) _typeSynonymMap}
        ifor_ (dsState ^. kindCtx) $ \tv kind -> do
          when (tv.sort == External && tv.moduleName == moduleName) do
            modify \inf@Interface {..} ->
              inf {_kindCtx = insertKind tv kind _kindCtx}

toInterfacePath :: String -> FilePath
toInterfacePath x = replaceExtension x "mlgi"

loadInterface ::
  (HasCallStack, IOE :> es, State (HashMap ModuleName Interface) :> es, Reader ModulePathList :> es) =>
  ModuleName ->
  Eff es Interface
loadInterface (ModuleName modName) = do
  ModulePathList modulePaths <- ask
  interfaces <- get
  case HashMap.lookup (ModuleName modName) interfaces of
    Just interface -> pure interface
    Nothing -> do
      message <-
        firstJustM
          (liftIO . readFileIfExists (toInterfacePath $ convertString modName))
          modulePaths
      case message of
        Just x -> do
          modify $ HashMap.insert (ModuleName modName) x
          pure x
        Nothing -> do
          errorDoc $ "Cannot find module:" <+> squotes (pretty modName)
  where
    readFileIfExists file directory =
      ifM
        (liftIO $ Directory.doesFileExist (directory </> file))
        (Just <$> liftIO (decodeEx <$> BS.readFile (directory </> file)))
        (pure Nothing)
        `catch` \(_ :: IOException) -> do
          _ <- warningDoc $ "Cannot read interface file:" <+> squotes (pretty file)
          readFileIfExists file directory

warningDoc :: Doc x -> IO ()
warningDoc doc = hPutTextLn stderr $ render $ "Warning:" <+> doc

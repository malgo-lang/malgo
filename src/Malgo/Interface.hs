{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Interface (Interface (..), ModulePathList (..), coreIdentMap, buildInterface, toInterfacePath, loadInterface) where

import Control.Exception (IOException, catch)
import Control.Lens (ifor_, (^.))
import Control.Lens.TH
import Data.Binary (Binary, decodeFile)
import Data.HashMap.Strict qualified as HashMap
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
  { -- | Used in Infer
    _signatureMap :: HashMap RnId (GT.Scheme GT.Type),
    -- | Used in Infer
    _typeDefMap :: HashMap RnId (GT.TypeDef GT.Type),
    -- | Used in Infer
    _typeSynonymMap :: HashMap GT.TypeVar ([GT.TypeVar], GT.Type),
    _kindCtx :: KindCtx,
    -- | Used in Rename
    _resolvedVarIdentMap :: HashMap PsId RnId,
    -- | Used in Rename
    _resolvedTypeIdentMap :: HashMap PsId RnId,
    -- | Used in Desugar
    _coreIdentMap :: HashMap RnId (Id C.Type),
    -- | Used in Rename
    infixMap :: HashMap RnId (Assoc, Int),
    -- | Used in Rename
    dependencies :: HashSet ModuleName
  }
  deriving stock (Show, Generic)

instance Binary Interface

makeFieldsNoPrefix ''Interface

instance Pretty Interface where
  pretty = viaShow

buildInterface :: ModuleName -> RnState -> DsState -> Interface
-- TODO: write abbrMap to interface
buildInterface moduleName rnState dsState = runPureEff $ execState (Interface mempty mempty mempty mempty mempty mempty mempty (rnState ^. RnState.infixInfo) (rnState ^. RnState.dependencies)) $ do
  ifor_ (dsState ^. nameEnv) $ \tcId coreId ->
    when (tcId.sort == External && tcId.moduleName == moduleName) do
      modify \inf@Interface {..} ->
        inf
          { _resolvedVarIdentMap = HashMap.insert tcId.name tcId _resolvedVarIdentMap,
            _coreIdentMap = HashMap.insert tcId coreId _coreIdentMap
          }
  ifor_ (dsState ^. signatureMap) $ \tcId scheme ->
    when (tcId.sort == External && tcId.moduleName == moduleName) do
      modify \inf@Interface {..} ->
        inf {_signatureMap = HashMap.insert tcId scheme _signatureMap}
  ifor_ (dsState ^. typeDefMap) $ \rnId typeDef -> do
    when (rnId.sort == External && rnId.moduleName == moduleName) do
      modify \inf@Interface {..} ->
        inf
          { _resolvedTypeIdentMap = HashMap.insert rnId.name rnId _resolvedTypeIdentMap,
            _typeDefMap = HashMap.insert rnId typeDef _typeDefMap
          }
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
        (Just <$> liftIO (decodeFile (directory </> file)))
        (pure Nothing)
        `catch` \(_ :: IOException) -> do
          _ <- warningDoc $ "Cannot read interface file:" <+> squotes (pretty file)
          readFileIfExists file directory

warningDoc :: Doc x -> IO ()
warningDoc doc = hPutTextLn stderr $ render $ "Warning:" <+> doc

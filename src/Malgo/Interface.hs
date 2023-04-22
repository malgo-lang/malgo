module Malgo.Interface (Interface (..), buildInterface, toInterfacePath, loadInterface) where

import Control.Lens (ifor_)
import Control.Monad.Extra (firstJustM, ifM)
import Data.Binary (Binary, decodeFile)
import Data.HashMap.Strict qualified as HashMap
import GHC.Records (HasField)
import Koriel.Core.Type qualified as C
import Koriel.Id
import Koriel.Pretty
import Malgo.Infer.TypeRep (KindCtx, insertKind)
import Malgo.Infer.TypeRep qualified as GT
import Malgo.Prelude
import Malgo.Syntax.Extension
import System.Directory qualified as Directory
import System.FilePath (replaceExtension, (</>))

data Interface = Interface
  { -- | Used in Infer
    signatureMap :: HashMap RnId (GT.Scheme GT.Type),
    -- | Used in Infer
    typeDefMap :: HashMap RnId (GT.TypeDef GT.Type),
    -- | Used in Infer
    typeSynonymMap :: HashMap GT.TypeVar ([GT.TypeVar], GT.Type),
    kindCtx :: KindCtx,
    -- | Used in Rename
    resolvedVarIdentMap :: HashMap PsId RnId,
    -- | Used in Rename
    resolvedTypeIdentMap :: HashMap PsId RnId,
    -- | Used in Desugar
    coreIdentMap :: HashMap RnId (Id C.Type),
    -- | Used in Rename
    infixMap :: HashMap RnId (Assoc, Int),
    -- | Used in Rename
    dependencies :: HashSet ModuleName
  }
  deriving stock (Show, Generic)

instance Binary Interface

instance Pretty Interface where
  pPrint = Koriel.Pretty.text . show

buildInterface ::
  ( HasField "_infixInfo" r (HashMap RnId (Assoc, Int)),
    HasField "_dependencies" r (HashSet ModuleName),
    HasField "_nameEnv" d (HashMap RnId (Id C.Type)),
    HasField "_signatureMap" d (HashMap RnId (GT.Scheme GT.Type)),
    HasField "_typeDefMap" d (HashMap RnId (GT.TypeDef GT.Type)),
    HasField "_kindCtx" d KindCtx
  ) =>
  ModuleName ->
  r ->
  d ->
  Interface
-- TODO: write abbrMap to interface
buildInterface moduleName rnState dsState = execState ?? Interface mempty mempty mempty mempty mempty mempty mempty (rnState._infixInfo) (rnState._dependencies) $ do
  ifor_ dsState._nameEnv $ \tcId coreId ->
    when (tcId.sort == External && tcId.moduleName == moduleName) do
      -- resolvedVarIdentMap . at (tcId.name) ?= tcId
      modify $ \e -> e {resolvedVarIdentMap = HashMap.insert (tcId.name) tcId e.resolvedVarIdentMap}
      -- coreIdentMap . at tcId ?= coreId
      modify $ \e -> e {coreIdentMap = HashMap.insert tcId coreId e.coreIdentMap}
  ifor_ dsState._signatureMap $ \tcId scheme ->
    when (tcId.sort == External && tcId.moduleName == moduleName) do
      -- signatureMap . at tcId ?= scheme
      modify $ \e -> e {signatureMap = HashMap.insert tcId scheme e.signatureMap}
  ifor_ dsState._typeDefMap $ \rnId typeDef -> do
    when (rnId.sort == External && rnId.moduleName == moduleName) do
      -- resolvedTypeIdentMap . at (rnId.name) ?= rnId
      modify $ \e -> e {resolvedTypeIdentMap = HashMap.insert (rnId.name) rnId e.resolvedTypeIdentMap}
      -- typeDefMap . at rnId ?= typeDef
      modify $ \e -> e {typeDefMap = HashMap.insert rnId typeDef e.typeDefMap}
  ifor_ dsState._kindCtx $ \tv kind -> do
    when (tv.sort == External && tv.moduleName == moduleName) do
      -- kindCtx %= insertKind tv kind
      modify $ \e -> e {kindCtx = insertKind tv kind e.kindCtx}

toInterfacePath :: String -> FilePath
toInterfacePath x = replaceExtension x "mlgi"

loadInterface ::
  ( HasCallStack,
    MonadReader s m,
    MonadIO m,
    HasField "_interfaces" s (IORef (HashMap ModuleName Interface)),
    HasField "_modulePaths" s [FilePath]
  ) =>
  ModuleName ->
  m Interface
loadInterface (ModuleName modName) = do
  interfacesRef <- asks (._interfaces)
  interfaces <- liftIO $ readIORef interfacesRef
  case HashMap.lookup (ModuleName modName) interfaces of
    Just interface -> pure interface
    Nothing -> do
      message <-
        firstJustM
          (readFileIfExists (toInterfacePath $ convertString modName))
          =<< asks (._modulePaths)
      case message of
        Just x -> do
          liftIO $ writeIORef interfacesRef $ HashMap.insert (ModuleName modName) x interfaces
          pure x
        Nothing -> do
          errorDoc $ "Cannot find module:" <+> quotes (pPrint modName)
  where
    readFileIfExists file directory =
      ifM
        (liftIO $ Directory.doesFileExist (directory </> file))
        (Just <$> liftIO (decodeFile (directory </> file)))
        (pure Nothing)

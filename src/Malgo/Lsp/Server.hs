module Malgo.Lsp.Server where

import Control.Lens (to, view, (^.))
import qualified Data.HashMap.Strict as HashMap
import Koriel.Id
import Koriel.Lens (HasSymbolInfo (..))
import Koriel.Pretty (Pretty (pPrint), render, (<+>))
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import Malgo.Interface
import Malgo.Lsp.Index (Info (..), findInfosOfPos)
import Malgo.Lsp.Pass (LspOpt)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude hiding (Range)
import Malgo.Syntax
import Malgo.Syntax.Extension
import qualified Relude.Unsafe as Unsafe
import System.FilePath (dropExtensions, takeFileName)
import Text.Megaparsec (errorBundlePretty)

textDocumentIdentifierToModuleName :: TextDocumentIdentifier -> ModuleName
textDocumentIdentifierToModuleName (uriToFilePath . view uri -> Just filePath) =
  ModuleName $ toText $ takeFileName $ dropExtensions filePath
textDocumentIdentifierToModuleName _ = error "textDocumentIdentifierToModuleName: invalid TextDocumentIdentifier"

readAst :: MonadIO m => LspOpt -> FilePath -> m (Module (Malgo 'Parse))
readAst _opt filePath = do
  src <- decodeUtf8 <$> readFileBS filePath
  case parseMalgo filePath src of
    Left err -> error $ toText $ "readAst: " <> errorBundlePretty err
    Right ast -> pure ast

handlers :: LspOpt -> Handlers (LspM ())
handlers opt =
  mconcat
    [ notificationHandler SInitialized $ \_notification -> do
        liftIO $ hPutStrLn stderr "Initialized",
      -- textDocument/hover
      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
        _ast <- readAst opt (Unsafe.fromJust $ doc ^. uri . to uriToFilePath)
        minterface <- liftIO $ runReaderT (loadInterface $ textDocumentIdentifierToModuleName doc) opt
        let index = case minterface of
              Nothing -> mempty
              Just interface -> interface ^. lspIndex
        let infos = findInfosOfPos (positionToSourcePos (Unsafe.fromJust $ doc ^. uri . to uriToFilePath) pos) index
        case infos of
          [] -> responder (Right Nothing)
          _ -> do
            let Position _l _c' = pos
                rsp = Hover ms (Just range)
                ms = HoverContents $ toHoverDocument infos
                range = Range pos pos
            responder (Right $ Just rsp),
      -- textDocument/definition
      requestHandler STextDocumentDefinition $ \req responder -> do
        let RequestMessage _ _ _ (DefinitionParams doc pos _workDone _partialResult) = req
        _ast <- readAst opt (Unsafe.fromJust $ doc ^. uri . to uriToFilePath)
        minterface <- liftIO $ runReaderT (loadInterface $ textDocumentIdentifierToModuleName doc) opt
        let index = case minterface of
              Nothing -> mempty
              Just interface -> interface ^. lspIndex
        let infos = findInfosOfPos (positionToSourcePos (Unsafe.fromJust $ doc ^. uri . to uriToFilePath) pos) index
        case infos of
          [] -> responder (Right $ InR $ InL $ Language.LSP.Types.List [])
          _ -> do
            let rsp = InR (InL $ Language.LSP.Types.List $ concatMap infoToLocation infos)
            responder (Right rsp),
      requestHandler STextDocumentDocumentSymbol $ \req responder -> do
        let RequestMessage _ _ _ (DocumentSymbolParams _ _ doc) = req
        minterface <- liftIO $ runReaderT (loadInterface $ textDocumentIdentifierToModuleName doc) opt
        let index = case minterface of
              Nothing -> mempty
              Just interface -> interface ^. lspIndex
        let documentSymbol = HashMap.elems $ HashMap.filterWithKey (\k _ -> idIsExternal k) $ index ^. symbolInfo
        responder $ Right $ InL $ Language.LSP.Types.List documentSymbol
    ]

toHoverDocument :: [Info] -> MarkupContent
toHoverDocument infos =
  mconcat $ map aux infos
  where
    aux Info {..} =
      markedUpContent "malgo" (toText $ render $ pPrint _name <+> ":" <+> pPrint _typeSignature)
        <> unmarkedUpContent (toText $ render $ pPrint _definitions)

infoToLocation :: Info -> [Location]
infoToLocation Info {..} =
  map malgoRangeToLocation _definitions

server :: LspOpt -> IO Int
server opt =
  runServer $
    ServerDefinition
      { onConfigurationChange = \_ _ -> Right (),
        defaultConfig = (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = handlers opt,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }

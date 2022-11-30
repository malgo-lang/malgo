{-# OPTIONS_GHC -Wno-deprecations #-}

module Malgo.Lsp.Server (server) where

import Control.Lens (view, (^.))
import Data.HashMap.Strict qualified as HashMap
import Koriel.Id
import Koriel.Pretty (Pretty (pPrint), render, (<+>))
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import Malgo.Lsp.Index (HasSymbolInfo (symbolInfo), Index, Info (..), LspOpt, findReferences)
import Malgo.Lsp.Index qualified as Index
import Malgo.Prelude hiding (Range)
import Relude.Unsafe qualified as Unsafe
import System.FilePath (dropExtensions, takeFileName)

textDocumentIdentifierToModuleName :: TextDocumentIdentifier -> ModuleName
textDocumentIdentifierToModuleName (uriToFilePath . view uri -> Just filePath) =
  ModuleName $ toText $ takeFileName $ dropExtensions filePath
textDocumentIdentifierToModuleName _ = error "textDocumentIdentifierToModuleName: invalid TextDocumentIdentifier"

handlers :: LspOpt -> Handlers (LspM ())
handlers opt =
  mconcat
    [ notificationHandler SInitialized $ \_notification -> do
        liftIO $ hPutStrLn stderr "Initialized",
      -- textDocument/hover
      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
        index <- loadIndex doc opt
        case findReferences (positionToSourcePos (Unsafe.fromJust $ uriToFilePath $ doc ^. uri) pos) index of
          [] -> responder (Right Nothing)
          infos -> do
            let rsp = Hover ms (Just range)
                ms = HoverContents $ toHoverDocument infos
                range = Range pos pos
            responder (Right $ Just rsp),
      -- textDocument/definition
      requestHandler STextDocumentDefinition $ \req responder -> do
        let RequestMessage _ _ _ (DefinitionParams doc pos _workDone _partialResult) = req
        index <- loadIndex doc opt
        let infos = findReferences (positionToSourcePos (Unsafe.fromJust $ uriToFilePath $ doc ^. uri) pos) index
            rsp = InR $ InL $ Language.LSP.Types.List $ concatMap infoToLocation infos
        responder (Right rsp),
      requestHandler STextDocumentDocumentSymbol $ \req responder -> do
        let RequestMessage _ _ _ (DocumentSymbolParams _ _ doc) = req
        index <- loadIndex doc opt
        let documentSymbol = map toDocumentSymbol $ HashMap.elems $ HashMap.filterWithKey (\k _ -> idIsExternal k) $ index ^. symbolInfo
        responder $ Right $ InL $ Language.LSP.Types.List documentSymbol
    ]

toDocumentSymbol :: Index.Symbol -> DocumentSymbol
toDocumentSymbol Index.Symbol {..} =
  DocumentSymbol
    { _name = name,
      _detail = Nothing,
      _kind = toKind kind,
      _tags = Nothing,
      _deprecated = Nothing,
      _range = malgoRangeToLspRange range,
      _selectionRange = malgoRangeToLspRange range,
      _children = Nothing
    }
  where
    toKind Index.Data = SkEnum
    toKind Index.TypeParam = SkTypeParameter
    toKind Index.Constructor = SkEnumMember
    toKind Index.Function = SkFunction
    toKind Index.Variable = SkVariable

loadIndex :: MonadIO f => TextDocumentIdentifier -> LspOpt -> f Index
loadIndex doc opt = maybe mempty identity <$> runReaderT (Index.loadIndex (textDocumentIdentifierToModuleName doc)) opt

toHoverDocument :: [Info] -> MarkupContent
toHoverDocument infos =
  mconcat $ map aux infos
  where
    aux Info {..} =
      markedUpContent "malgo" (toText $ render $ pPrint _name <+> ":" <+> pPrint typeSignature)
        <> unmarkedUpContent (toText $ render $ pPrint definitions)

infoToLocation :: Info -> [Location]
infoToLocation Info {..} =
  map malgoRangeToLocation definitions

server :: LspOpt -> IO Int
server opt = do
  runServer $
    ServerDefinition
      { onConfigurationChange = \_ _ -> Right (),
        defaultConfig = (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = handlers opt,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }

{-# LANGUAGE TemplateHaskell #-}

module Malgo.Lsp.Server where

import Control.Lens (to, view, (^.))
import Control.Lens.TH (makeFieldsNoPrefix)
import Koriel.Id
import Koriel.Lens
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import Malgo.Interface
import Malgo.Lsp.Index (findInfosOfPos, Info)
import Malgo.Parser (parseMalgo)
import Malgo.Prelude hiding (Range)
import Malgo.Syntax
import Malgo.Syntax.Extension
import qualified Relude.Unsafe as Unsafe
import System.FilePath (dropExtensions, takeFileName)
import Text.Megaparsec (errorBundlePretty, SourcePos (SourcePos), mkPos)
import Text.Megaparsec.Pos (SourcePos)
import Koriel.Pretty (render, Pretty (pPrint))
import qualified Koriel.Pretty as P

newtype LspEnv = LspEnv
  { _opt :: Opt
  }

makeFieldsNoPrefix ''LspEnv

textDocumentIdentifierToModuleName :: TextDocumentIdentifier -> ModuleName
textDocumentIdentifierToModuleName (uriToFilePath . view uri -> Just filePath) =
  ModuleName $ toText $ takeFileName $ dropExtensions filePath
textDocumentIdentifierToModuleName _ = error "textDocumentIdentifierToModuleName: invalid TextDocumentIdentifier"

readAst :: MonadIO m => Opt -> FilePath -> m (Module (Malgo 'Parse))
readAst _opt filePath = do
  src <- readFileText filePath
  case parseMalgo filePath src of
    Left err -> error $ toText $ "readAst: " <> errorBundlePretty err
    Right ast -> pure ast

handlers :: Opt -> Handlers (LspM ())
handlers opt =
  mconcat
    [ notificationHandler SInitialized $ \_notification -> do
        liftIO $ hPutStrLn stderr "Initialized",
      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams doc pos _workDone) = req
        _ast <- readAst opt (Unsafe.fromJust $ doc ^. uri . to uriToFilePath)
        minterface <- liftIO $ runReaderT (loadInterface $ textDocumentIdentifierToModuleName doc) (LspEnv opt)
        let index = case minterface of
              Nothing -> mempty
              Just interface -> interface ^. lspIndex
        let infos = findInfosOfPos (convertPos (Unsafe.fromJust $ doc ^. uri . to uriToFilePath) pos) index
        let Position _l _c' = pos
            rsp = Hover ms (Just range)
            ms = HoverContents $ markedUpContent "malgo" (toHoverDocument infos)
            range = Range pos pos
        responder (Right $ Just rsp)
    ]

convertPos :: FilePath -> Position -> SourcePos
convertPos srcName Position {_line, _character} = SourcePos srcName (mkPos $ fromIntegral _line + 1) (mkPos $ fromIntegral _character + 1)

toHoverDocument :: [Info] -> Text
toHoverDocument infos = toText $ render $ P.sep $ P.punctuate ";" $ map pPrint infos

server :: Opt -> IO Int
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

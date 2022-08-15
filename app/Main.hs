module Main where

import Control.Lens ((.~), (<>~))
import Koriel.Id (ModuleName)
import Koriel.Lens (HasModulePaths (..))
import Malgo.Build qualified as Build
import Malgo.Driver qualified as Driver
import Malgo.Interface
import Malgo.Lsp.Pass (LspOpt (..))
import Malgo.Lsp.Server qualified as Lsp
import Malgo.Prelude hiding (toLLOpt)
import Options.Applicative
import System.Directory (XdgDirectory (XdgData), getXdgDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.FilePath.Lens (extension)
import Text.Read (read)

main :: IO ()
main = do
  command <- parseCommand
  case command of
    ToLL opt -> do
      basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      opt <- pure $ opt & modulePaths <>~ [".malgo-work" </> "build", basePath]
      Driver.compile opt
    Lsp opt -> do
      basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      opt <- pure $ opt & modulePaths <>~ [".malgo-work" </> "build", basePath]
      void $ Lsp.server opt
    Build _ -> do
      Build.run

toLLOpt :: Parser ToLLOpt
toLLOpt =
  ( ToLLOpt
      <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")
      <*> strOption
        ( long "output"
            <> short 'o'
            <> metavar "OUTPUT"
            <> value ""
            <> help
              "Write LLVM IR to OUTPUT"
        )
      <*> switch (long "dump-parsed")
      <*> switch (long "dump-renamed")
      <*> switch (long "dump-typed")
      <*> switch (long "dump-refine")
      <*> switch (long "dump-desugar")
      <*> switch (long "no-opt")
      <*> switch (long "no-lambdalift")
      <*> fmap read (strOption (long "inline" <> value "10"))
      <*> switch (long "debug-mode")
      <*> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH"))
      <*> switch (long "force")
  )
    <**> helper

lspOpt :: IORef (HashMap ModuleName Interface) -> Parser LspOpt
lspOpt ref = LspOpt <$> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH")) <*> pure ref <**> helper

data BuildOpt = BuildOpt
  deriving stock (Eq, Show)

data Command
  = ToLL ToLLOpt
  | Lsp LspOpt
  | Build BuildOpt

parseCommand :: IO Command
parseCommand = do
  interfacesRef <- newIORef mempty
  command <-
    execParser
      ( info ((subparser toLL <|> subparser (lsp interfacesRef) <|> subparser build) <**> helper) $
          fullDesc
            <> header "malgo programming language"
      )
  case command of
    ToLL opt -> do
      srcName <- makeAbsolute opt._srcName
      if null opt._dstName
        then pure $ ToLL $ opt {_srcName = srcName, _dstName = srcName & extension .~ ".ll"}
        else pure $ ToLL $ opt {_srcName = srcName}
    Lsp opt -> pure $ Lsp opt
    Build opt -> pure $ Build opt
  where
    toLL =
      command "to-ll" $
        info (ToLL <$> toLLOpt) $
          fullDesc
            <> progDesc "Compile Malgo file (.mlg) to LLVM Textual IR (.ll)"
            <> header "malgo to LLVM Textual IR Compiler"
    lsp ref =
      command "lsp" $
        info (Lsp <$> lspOpt ref) $
          fullDesc
            <> progDesc "Language Server for Malgo"
            <> header "Malgo Language Server"
    build =
      command "build" $
        info (Build <$> buildOpt) $
          fullDesc
            <> progDesc "Build Malgo program"
            <> header "malgo build"
    buildOpt = pure BuildOpt

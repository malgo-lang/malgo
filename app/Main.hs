{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (makeFieldsNoPrefix, (.~), (<>~))
import Koriel.Id (ModuleName)
import Koriel.Lens (HasModulePaths (..))
import Koriel.MonadUniq (UniqSupply (UniqSupply))
import Malgo.Build qualified as Build
import Malgo.Driver qualified as Driver
import Malgo.Lsp.Index (Index)
import Malgo.Lsp.Pass (LspOpt (..))
import Malgo.Lsp.Server qualified as Lsp
import Malgo.Prelude hiding (MalgoEnv (..))
import Malgo.Prelude qualified as Prelude
import Options.Applicative
import System.Directory (XdgDirectory (XdgData), getXdgDirectory, makeAbsolute)
import System.FilePath (takeDirectory, (-<.>), (</>))
import System.FilePath.Lens (extension)
import Text.Read (read)

data ToLLOpt = ToLLOpt
  { srcPath :: FilePath,
    _dstPath :: FilePath,
    _compileMode :: CompileMode,
    _noOptimize :: Bool,
    lambdaLift :: Bool,
    _inlineSize :: Int,
    debugMode :: Bool,
    _modulePaths :: [FilePath]
  }
  deriving stock (Eq, Show)

makeFieldsNoPrefix ''ToLLOpt

main :: IO ()
main = do
  command <- parseCommand
  case command of
    ToLL opt -> do
      basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      opt <- pure $ opt & modulePaths <>~ [takeDirectory opt._dstPath, ".malgo-work" </> "build", basePath]
      _uniqSupply <- UniqSupply <$> newIORef 0
      _interfaces <- newIORef mempty
      _indexes <- newIORef mempty
      let ToLLOpt {..} = opt
      Driver.compile opt.srcPath Prelude.MalgoEnv {..}
    Lsp opt -> do
      basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      opt <- pure $ opt & modulePaths <>~ [".malgo-work" </> "build", basePath]
      void $ Lsp.server opt
    Build _ -> do
      Build.run

defaultToLLOpt :: FilePath -> ToLLOpt
defaultToLLOpt src =
  ToLLOpt
    { srcPath = src,
      _dstPath = src -<.> "ll",
      _compileMode = LLVM,
      _noOptimize = False,
      lambdaLift = False,
      _inlineSize = 15,
      debugMode = False,
      _modulePaths = []
    }

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
      <*> flag
        LLVM
        Scheme
        ( long "scheme"
            <> short 's'
            <> help
              "Compile to Scheme instead of LLVM"
        )
      <*> switch (long "no-opt")
      <*> switch (long "lambdalift")
      <*> fmap read (strOption (long "inline" <> value "15"))
      <*> switch (long "debug-mode")
      <*> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH"))
  )
    <**> helper

lspOpt :: IORef (HashMap ModuleName Index) -> Parser LspOpt
lspOpt cache = LspOpt <$> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH")) <*> pure cache <**> helper

data BuildOpt = BuildOpt
  deriving stock (Eq, Show)

data Command
  = ToLL ToLLOpt
  | Lsp LspOpt
  | Build BuildOpt

parseCommand :: IO Command
parseCommand = do
  cache <- newIORef mempty
  command <-
    execParser
      ( info ((subparser toLL <|> subparser (lsp cache) <|> subparser build) <**> helper) $
          fullDesc
            <> header "malgo programming language"
      )
  case command of
    ToLL opt -> do
      srcPath <- makeAbsolute opt.srcPath
      if null opt._dstPath
        then pure $ ToLL $ opt {srcPath = srcPath, _dstPath = srcPath & extension .~ ".ll"}
        else pure $ ToLL $ opt {srcPath = srcPath}
    Lsp opt -> pure $ Lsp opt
    Build opt -> pure $ Build opt
  where
    toLL =
      command "to-ll" $
        info (ToLL <$> toLLOpt) $
          fullDesc
            <> progDesc "Compile Malgo file (.mlg) to LLVM Textual IR (.ll)"
            <> header "malgo to LLVM Textual IR Compiler"
    lsp cache = do
      command "lsp" $
        info (Lsp <$> lspOpt cache) $
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

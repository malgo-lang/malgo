{-# LANGUAGE TemplateHaskell #-}
-- For use of 'undefined'
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Lens (makeFieldsNoPrefix, (.~), (<>~))
import Error.Diagnose (addFile, defaultStyle, printDiagnostic)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Koriel.Core.Parser qualified as Koriel
import Koriel.Id (ModuleName)
import Koriel.Lens (HasModulePaths (..))
import Koriel.Pretty (pPrint)
import Malgo.Build qualified as Build
import Malgo.Driver qualified as Driver
import Malgo.Lsp.Index (Index, LspOpt (..))
import Malgo.Lsp.Server qualified as Lsp
import Malgo.Monad (CompileMode (..), newMalgoEnv)
import Malgo.Monad qualified as Monad
import Malgo.Prelude
import Options.Applicative
import System.Directory (XdgDirectory (XdgData), getXdgDirectory, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.FilePath.Lens (extension)

data ToLLOpt = ToLLOpt
  { srcPath :: FilePath,
    dstPath :: FilePath,
    compileMode :: CompileMode,
    noOptimize :: Bool,
    lambdaLift :: Bool,
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
      -- basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      opt <- pure $ opt & modulePaths <>~ [takeDirectory opt.dstPath]
      -- _uniqSupply <- UniqSupply <$> newIORef 0
      -- _interfaces <- newIORef mempty
      -- _indexes <- newIORef mempty
      -- let ToLLOpt {..} = opt
      env <- newMalgoEnv opt.srcPath opt._modulePaths Nothing undefined Nothing Nothing
      Driver.compile opt.srcPath env {Monad.dstPath = opt.dstPath, Monad.compileMode = opt.compileMode, Monad.noOptimize = opt.noOptimize, Monad.lambdaLift = opt.lambdaLift, Monad.debugMode = opt.debugMode}
    Lsp opt -> do
      basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      opt <- pure $ opt & modulePaths <>~ [".malgo-work" </> "build", basePath]
      void $ Lsp.server opt
    Build _ -> do
      Build.run
    Koriel (KorielOpt srcPath) -> do
      srcContents <- readFileBS srcPath
      case Koriel.parse srcPath (decodeUtf8 srcContents) of
        Left err ->
          let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
              diag' = addFile diag srcPath (decodeUtf8 srcContents)
           in printDiagnostic stderr True True 4 defaultStyle diag' >> exitFailure
        Right prog -> do
          print $ pPrint prog

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
      <*> pure LLVM
      <*> switch (long "no-opt")
      <*> switch (long "lambdalift")
      <*> switch (long "debug-mode")
      <*> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH"))
  )
    <**> helper

lspOpt :: IORef (HashMap ModuleName Index) -> Parser LspOpt
lspOpt cache = LspOpt <$> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH")) <*> pure cache <**> helper

data BuildOpt = BuildOpt
  deriving stock (Eq, Show)

data KorielOpt = KorielOpt FilePath
  deriving stock (Eq, Show)

data Command
  = ToLL ToLLOpt
  | Lsp LspOpt
  | Build BuildOpt
  | Koriel KorielOpt

parseCommand :: IO Command
parseCommand = do
  cache <- newIORef mempty
  command <-
    execParser
      ( info ((subparser toLL <|> subparser (lsp cache) <|> subparser build <|> subparser koriel) <**> helper) $
          fullDesc
            <> header "malgo programming language"
      )
  case command of
    ToLL opt -> do
      srcPath <- makeAbsolute opt.srcPath
      if null opt.dstPath
        then pure $ ToLL $ opt {srcPath = srcPath, dstPath = srcPath & extension .~ ".ll"}
        else pure $ ToLL $ opt {srcPath = srcPath}
    Lsp opt -> pure $ Lsp opt
    Build opt -> pure $ Build opt
    Koriel opt -> pure $ Koriel opt
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
    koriel =
      command "koriel" $
        info (Koriel <$> korielOpt) $
          fullDesc
            <> progDesc "Koriel Compiler"
            <> header "malgo koriel"
    korielOpt = KorielOpt <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")

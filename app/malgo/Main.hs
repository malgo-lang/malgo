{-# LANGUAGE TemplateHaskell #-}
-- For use of 'undefined'
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Lens (makeFieldsNoPrefix, (.~))
import Data.ByteString qualified as BS
import Effectful
import Error.Diagnose (TabSize (..), WithUnicode (..), addFile, defaultStyle, printDiagnostic)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Malgo.Core.Optimize (OptimizeOption (..))
import Malgo.Core.Parser qualified as Core
import Malgo.Driver qualified as Driver
import Malgo.Monad (CompileMode (..), runMalgoM)
import Malgo.Monad qualified as Flag
import Malgo.Prelude
import Options.Applicative
import System.Directory (makeAbsolute)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.FilePath.Lens (extension)

data ToLLOpt = ToLLOpt
  { srcPath :: FilePath,
    dstPath :: FilePath,
    compileMode :: CompileMode,
    noOptimize :: Bool,
    lambdaLift :: Bool,
    optimizeOption :: OptimizeOption,
    debugMode :: Bool,
    modulePaths :: [FilePath]
  }

makeFieldsNoPrefix ''ToLLOpt

main :: IO ()
main = do
  command <- parseCommand
  case command of
    ToLL opt -> do
      opt <- pure $ opt {modulePaths = opt.modulePaths <> [takeDirectory opt.dstPath]}
      runEff
        $ Driver.compile opt.srcPath
        & runMalgoM
          opt.srcPath
          opt.compileMode
          Flag
            { Flag.noOptimize = opt.noOptimize,
              Flag.lambdaLift = opt.lambdaLift,
              Flag.debugMode = opt.debugMode,
              Flag.testMode = False
            }
          opt.optimizeOption
    Core (CoreOpt srcPath) -> do
      srcContents <- BS.readFile srcPath
      case Core.parse srcPath (convertString srcContents) of
        Left err ->
          let diag = errorDiagnosticFromBundle @Text Nothing "Parse error on input" Nothing err
              diag' = addFile diag srcPath (convertString srcContents)
           in printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle diag' >> exitFailure
        Right prog -> do
          putText $ render $ pretty prog

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
      <*> ( strOption (long "compile-mode" <> short 'c' <> metavar "COMPILE_MODE" <> value "llvm") <&> \case
              ("llvm" :: String) -> LLVM
              _ -> error "Invalid compile mode"
          )
      <*> switch (long "no-opt")
      <*> switch (long "lambdalift")
      <*> ( OptimizeOption
              <$> switch (long "ffold-variable")
              <*> switch (long "finline-constructor")
              <*> switch (long "feliminate-unused-let")
              <*> switch (long "finline-function")
              <*> (read <$> strOption (long "finline-threshold" <> metavar "INT" <> value "10"))
              <*> switch (long "ffold-redundant-cast")
              <*> switch (long "ffold-trivial-call")
              <*> (not <$> switch (long "fspecialize-function"))
              <*> switch (long "fremove-noop-destruct")
          )
      <*> switch (long "debug-mode")
      <*> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH"))
  )
    <**> helper

newtype CoreOpt = CoreOpt FilePath
  deriving stock (Eq, Show)

data Command
  = ToLL ToLLOpt
  | Core CoreOpt

parseCommand :: IO Command
parseCommand = do
  command <-
    execParser
      ( info ((subparser toLL <|> subparser core) <**> helper)
          $ fullDesc
          <> header "malgo programming language"
      )
  case command of
    ToLL opt -> do
      srcPath <- makeAbsolute opt.srcPath
      if null opt.dstPath
        then pure $ ToLL $ opt {srcPath = srcPath, dstPath = srcPath & extension .~ ".ll"}
        else pure $ ToLL $ opt {srcPath = srcPath}
    Core opt -> pure $ Core opt
  where
    toLL =
      command "to-ll"
        $ info (ToLL <$> toLLOpt)
        $ fullDesc
        <> progDesc "Compile Malgo file (.mlg) to LLVM Textual IR (.ll)"
        <> header "malgo to LLVM Textual IR Compiler"
    core =
      command "core"
        $ info (Core <$> coreOpt)
        $ fullDesc
        <> progDesc "Core Compiler"
        <> header "malgo core"
    coreOpt = CoreOpt <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")

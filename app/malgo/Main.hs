{-# LANGUAGE TemplateHaskell #-}
-- For use of 'undefined'
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Data.ByteString qualified as BS
import Malgo.Core.Optimize (OptimizeOption (..))
import Malgo.Core.Parser qualified as Core
import Malgo.Driver qualified as Driver
import Malgo.Monad (runMalgoM)
import Malgo.Monad qualified as Flag
import Malgo.Prelude
import Options.Applicative
import System.Directory (makeAbsolute)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)

data EvalOpt = ToLLOpt
  { srcPath :: FilePath,
    noOptimize :: Bool,
    lambdaLift :: Bool,
    optimizeOption :: OptimizeOption,
    debugMode :: Bool
  }

main :: IO ()
main = do
  command <- parseCommand
  case command of
    Eval opt ->
      Driver.compile opt.srcPath
        & runMalgoM
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
        Left err -> do
          hPutStrLn stderr $ errorBundlePretty err
          exitFailure
        Right prog -> do
          putText $ render $ pretty prog

evalOpt :: Parser EvalOpt
evalOpt =
  ( ToLLOpt
      <$> strArgument (metavar "SOURCE" <> help "Source file (relative path)" <> action "file")
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
  )
    <**> helper

newtype CoreOpt = CoreOpt FilePath
  deriving stock (Eq, Show)

data Command
  = Eval EvalOpt
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
    Eval opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Eval opt {srcPath = srcPath}
    Core opt -> pure $ Core opt
  where
    toLL =
      command "eval"
        $ info (Eval <$> evalOpt)
        $ fullDesc
        <> progDesc "Evaluate a malgo program"
    core =
      command "core"
        $ info (Core <$> coreOpt)
        $ fullDesc
        <> progDesc "Core Compiler"
        <> header "malgo core"
    coreOpt = CoreOpt <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")

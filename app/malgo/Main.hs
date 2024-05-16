{-# LANGUAGE TemplateHaskell #-}
-- For use of 'undefined'
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Lens (makeFieldsNoPrefix)
import Malgo.Core.Optimize (OptimizeOption (..))
import Malgo.Driver qualified as Driver
import Malgo.Monad (CompileMode (..), runMalgoM)
import Malgo.Monad qualified as Flag
import Malgo.Prelude
import Options.Applicative
import System.Directory (makeAbsolute)

data ToLLOpt = ToLLOpt
  { srcPath :: FilePath,
    compileMode :: CompileMode,
    noOptimize :: Bool,
    lambdaLift :: Bool,
    optimizeOption :: OptimizeOption,
    debugMode :: Bool,
    exitAfterDesugar :: Bool
  }

makeFieldsNoPrefix ''ToLLOpt

main :: IO ()
main = do
  command <- parseCommand
  case command of
    ToLL opt ->
      Driver.compile opt.srcPath
        & runMalgoM
          opt.compileMode
          Flag
            { Flag.noOptimize = opt.noOptimize,
              Flag.lambdaLift = opt.lambdaLift,
              Flag.debugMode = opt.debugMode,
              Flag.testMode = False,
              Flag.exitAfterDesugar = opt.exitAfterDesugar
            }
          opt.optimizeOption

toLLOpt :: Parser ToLLOpt
toLLOpt =
  ( ToLLOpt
      <$> strArgument (metavar "SOURCE" <> help "Source file (relative path)" <> action "file")
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
      <*> switch (long "only-desugar" <> help "Only generate .kor file and exit. Do not compile to .ll nor link Core files.")
  )
    <**> helper

newtype Command
  = ToLL ToLLOpt

parseCommand :: IO Command
parseCommand = do
  command <-
    execParser
      ( info (subparser toLL <**> helper)
          $ fullDesc
          <> header "malgo programming language"
      )
  case command of
    ToLL opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ ToLL opt {srcPath = srcPath}
  where
    toLL =
      command "to-ll"
        $ info (ToLL <$> toLLOpt)
        $ fullDesc
        <> progDesc "Compile Malgo file (.mlg) to LLVM Textual IR (.ll)"
        <> header "malgo to LLVM Textual IR Compiler"
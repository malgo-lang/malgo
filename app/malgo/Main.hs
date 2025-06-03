-- For use of 'undefined'
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Malgo.Driver qualified as Driver
import Malgo.Monad (runMalgoM)
import Malgo.Monad qualified as Flag
import Malgo.Prelude
import Options.Applicative
import System.Directory (makeAbsolute)

data EvalOpt = EvalOpt
  { srcPath :: FilePath,
    noOptimize :: Bool,
    lambdaLift :: Bool,
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

evalOpt :: Parser EvalOpt
evalOpt =
  ( EvalOpt
      <$> strArgument (metavar "SOURCE" <> help "Source file (relative path)" <> action "file")
      <*> switch (long "no-opt")
      <*> switch (long "lambdalift")
      <*> switch (long "debug-mode")
  )
    <**> helper

newtype Command
  = Eval EvalOpt

parseCommand :: IO Command
parseCommand = do
  command <-
    execParser
      ( info (subparser eval <**> helper)
          $ fullDesc
          <> header "malgo programming language"
      )
  case command of
    Eval opt -> do
      srcPath <- makeAbsolute opt.srcPath
      pure $ Eval opt {srcPath = srcPath}
  where
    eval =
      command "eval"
        $ info (Eval <$> evalOpt)
        $ fullDesc
        <> progDesc "Evaluate a malgo program"

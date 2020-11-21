{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Option where

import Koriel.Prelude
import Options.Applicative
import System.FilePath.Lens

data Opt = Opt
  { srcName :: String,
    dstName :: String,
    dumpParsed :: Bool,
    dumpRenamed :: Bool,
    dumpTyped :: Bool,
    dumpDesugar :: Bool,
    noOptimize :: Bool,
    noLambdaLift :: Bool,
    inlineSize :: Int,
    viaBinding :: Bool
  }
  deriving stock (Eq, Show)

parseOpt :: IO Opt
parseOpt = do
  opt <-
    execParser $
      info
        ( ( Opt
              <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")
              <*> strOption
                ( long "output" <> short 'o' <> metavar "OUTPUT" <> value ""
                    <> help
                      "Write LLVM IR to OUTPUT"
                )
              <*> switch (long "dump-parsed")
              <*> switch (long "dump-renamed")
              <*> switch (long "dump-typed")
              <*> switch (long "dump-desugar")
              <*> switch (long "no-lambdalift")
              <*> switch (long "no-opt")
              <*> fmap read (strOption (long "inline" <> value "10"))
              <*> switch (long "via-binding")
          )
            <**> helper
        )
        (fullDesc <> progDesc "griff" <> header "griff - a programming language")
  if null (dstName opt)
    then pure opt {dstName = srcName opt & extension .~ ".ll"}
    else pure opt

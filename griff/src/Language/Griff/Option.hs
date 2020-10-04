{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Option where

import Koriel.Prelude
import Options.Applicative

data Opt = Opt
  { srcName :: String,
    dstName :: String,
    dumpParsed :: Bool,
    dumpRenamed :: Bool,
    dumpTyped :: Bool,
    dumpDesugar :: Bool,
    noOptimize :: Bool,
    inlineSize :: Int
  }
  deriving stock (Eq, Show)

parseOpt :: IO Opt
parseOpt =
  execParser $
    info
      ( ( Opt
            <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")
            <*> strOption
              ( long "output" <> short 'o' <> metavar "OUTPUT" <> value "out.ll"
                  <> help
                    "Write LLVM IR to OUTPUT"
              )
            <*> switch (long "dump-parsed")
            <*> switch (long "dump-renamed")
            <*> switch (long "dump-typed")
            <*> switch (long "dump-desugar")
            <*> switch (long "no-opt")
            <*> fmap read (strOption (long "inline" <> value "10"))
        )
          <**> helper
      )
      (fullDesc <> progDesc "griff" <> header "griff - a programming language")

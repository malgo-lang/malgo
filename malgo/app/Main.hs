module Main where

import Development.Shake.FilePath ((</>))
import Language.Malgo.Prelude hiding (value)
import Options.Applicative
import Runner
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import System.FilePath.Lens (extension)

main :: IO ()
main = do
  command <- parseCommand
  case command of
    ToLL opt -> do
      basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      let opt' = opt {modulePaths = modulePaths opt <> [basePath]}
      runShakeRules ".malgo-work" opt' do
        -- modulePaths/*.mlgをすべてコンパイル
        buildUnderModulePaths opt'
        mlgToLLShake opt'

toLLOpt :: Parser Opt
toLLOpt =
  ( Opt
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
      <*> switch (long "no-opt")
      <*> switch (long "no-lambdalift")
      <*> fmap read (strOption (long "inline" <> value "10"))
      <*> switch (long "debug-mode")
      <*> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH"))
      <*> switch (long "force")
  )
    <**> helper

newtype Command = ToLL Opt

parseCommand :: IO Command
parseCommand = do
  command <-
    execParser
      ( info (subparser toLL <**> helper) $
          fullDesc
            <> header "malgo programming language"
      )
  case command of
    ToLL opt ->
      if null (dstName opt)
        then pure $ ToLL $ opt {dstName = srcName opt & extension .~ ".ll"}
        else pure command
  where
    toLL =
      command "to-ll" $
        info (ToLL <$> toLLOpt) $
          fullDesc
            <> progDesc "Compile Malgo file (.mlg) to LLVM Textual IR (.ll)"
            <> header "malgo to LLVM Textual IR Compiler"

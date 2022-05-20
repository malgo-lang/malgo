module Main where

import Control.Lens ((.~))
import qualified Data.Text.IO as T
import qualified Malgo.Driver as Driver
import qualified Malgo.Lsp.Server as Lsp
import Malgo.Parser (parseMalgo)
import Malgo.Prelude hiding (value)
import Options.Applicative
import System.Directory (XdgDirectory (XdgData), getXdgDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.FilePath.Lens (extension)
import Text.Megaparsec (errorBundlePretty)
import Text.Read (read)

main :: IO ()
main = do
  command <- parseCommand
  case command of
    ToLL opt -> do
      basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      opt <- pure $ opt {modulePaths = modulePaths opt <> [".malgo-work" </> "build", basePath]}
      src <- readFileText (srcName opt)
      let parsedAst = case parseMalgo (srcName opt) src of
            Right x -> x
            Left err -> error $ toText $ errorBundlePretty err
      Driver.compileFromAST parsedAst opt
    Lsp opt -> do
      basePath <- getXdgDirectory XdgData ("malgo" </> "base")
      opt <- pure $ opt {modulePaths = modulePaths opt <> [".malgo-work" </> "build", basePath]}
      void $ Lsp.server opt

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

lspOpt :: Parser Opt
lspOpt = toLLOpt

data Command
  = ToLL Opt
  | Lsp Opt

parseCommand :: IO Command
parseCommand = do
  command <-
    execParser
      ( info ((subparser toLL <|> subparser lsp) <**> helper) $
          fullDesc
            <> header "malgo programming language"
      )
  case command of
    ToLL opt -> do
      srcName <- makeAbsolute $ srcName opt
      if null (dstName opt)
        then pure $ ToLL $ opt {srcName = srcName, dstName = srcName & extension .~ ".ll"}
        else pure $ ToLL $ opt {srcName = srcName}
    Lsp opt -> do
      srcName <- makeAbsolute $ srcName opt
      if null $ dstName opt
        then pure $ Lsp $ opt {dstName = srcName & extension .~ ".ll"}
        else pure $ Lsp $ opt {srcName = srcName}
  where
    toLL =
      command "to-ll" $
        info (ToLL <$> toLLOpt) $
          fullDesc
            <> progDesc "Compile Malgo file (.mlg) to LLVM Textual IR (.ll)"
            <> header "malgo to LLVM Textual IR Compiler"
    lsp =
      command "lsp" $
        info (Lsp <$> lspOpt) $
          fullDesc
            <> progDesc "Language Server for Malgo"
            <> header "Malgo Language Server"

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Driver
  ( parseOpt,
    compile,
  )
where

import qualified Data.ByteString.Short as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Koriel.Core.CodeGen
import Koriel.Core.Core
  ( Atom (..),
    Exp (..),
    Program (..),
    Unboxed (..),
    appProgram,
    bind,
    mainFunc,
    runDef,
  )
import Koriel.Core.LambdaLift
import Koriel.Core.Lint
import Koriel.Core.Optimize
import Koriel.Prelude
import Koriel.Pretty
import qualified LLVM.AST as L
import Language.Malgo.FrontEnd.Rename
import Language.Malgo.FrontEnd.Typing.Infer
import Language.Malgo.IR.Syntax
import qualified Language.Malgo.Lexer as Lexer
import Language.Malgo.MiddleEnd.Desugar
import Language.Malgo.Monad as M
import qualified Language.Malgo.Parser as Parser
import Options.Applicative
import System.IO (hPrint, stderr)

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
            <*> switch (long "dump-type-table")
            <*> switch (long "dump-desugar")
            <*> switch (long "dump-lambdalift")
            <*> switch (long "dump-flat")
            <*> switch (long "debug-mode")
            <*> flag True False (long "no-lambdalift")
            <*> switch (long "no-opt")
            <*> fmap read (strOption (long "inline" <> value "10"))
            <*> switch (long "via-binding")
        )
          <**> helper
      )
      (fullDesc <> progDesc "malgo" <> header "malgo - a toy programming language")

readAndParse :: MalgoM (Expr String)
readAndParse = do
  opt <- asks maOption
  source <- asks maSource
  tokens <- Lexer.tokenize () (srcName opt) source
  let ast = case Parser.parseExpr <$> tokens of
        Left x -> error $ TL.unpack $ pShow x
        Right x -> x
  when (dumpParsed opt) $ liftIO $ hPrint stderr $ pPrint ast
  pure ast

withDump :: (MonadIO m, Pretty b) => Bool -> (t -> m b) -> t -> m b
withDump isDump m a = do
  a' <- m a
  when isDump $ liftIO $ hPrint stderr $ pPrint a'
  pure a'

compile :: MonadIO m => Opt -> Text -> m L.Module
compile = M.runMalgo $ do
  opt <- asks maOption
  expr <-
    readAndParse
      >>= withDump (dumpRenamed opt) rename
      >>= withDump (dumpTyped opt) typing
      >>= withDump (dumpDesugar opt) desugar
      >>= (\e -> lint e >> pure e)
      >>= withDump (dumpDesugar opt) (optimize (inlineSize opt))
  malgoMainFunc <-
    mainFunc =<< runDef do
      _ <- bind expr
      pure (Atom $ Unboxed $ Int32 0)
  let program = Program [malgoMainFunc]
  llvmir <-
    if applyLambdaLift opt
      then
        withDump (dumpLambdaLift opt) lambdalift program
          >>= traverseOf appProgram (withDump (dumpLambdaLift opt) (optimize (inlineSize opt)))
          >>= codeGen
      else codeGen program
  pure $
    L.defaultModule
      { L.moduleName = fromString $ srcName opt,
        L.moduleSourceFileName = B.toShort $ T.encodeUtf8 $ T.pack $ srcName opt,
        L.moduleDefinitions = llvmir
      }

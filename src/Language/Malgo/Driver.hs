{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Driver
  ( parseOpt,
    interpret,
    compile,
    compileCore,
  )
where

import qualified Data.ByteString.Short as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified LLVM.AST as L
import Language.Malgo.BackEnd.GenLIR
import Language.Malgo.BackEnd.GenLLVM
import Language.Malgo.Core.CodeGen
import Language.Malgo.Core.Eval
import Language.Malgo.Core.Flat
import Language.Malgo.Core.LambdaLift
import Language.Malgo.Core.Lint
import Language.Malgo.Core.Optimize
import Language.Malgo.FrontEnd.Rename
import Language.Malgo.FrontEnd.Typing.Infer
import Language.Malgo.IR.Syntax
import qualified Language.Malgo.Lexer as Lexer
import Language.Malgo.MiddleEnd.Desugar
import Language.Malgo.MiddleEnd.HIRLint
import Language.Malgo.MiddleEnd.MIRLint
import Language.Malgo.MiddleEnd.TransToHIR
import Language.Malgo.MiddleEnd.TransToMIR
import Language.Malgo.Monad as M
import qualified Language.Malgo.Parser as Parser
import Language.Malgo.Pass
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Options.Applicative

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
            <*> switch (long "dump-hir")
            <*> switch (long "dump-type-table")
            <*> switch (long "dump-mir")
            <*> switch (long "dump-lir")
            <*> switch (long "dump-desugar")
            <*> switch (long "dump-lambdalift")
            <*> switch (long "interpret")
            <*> switch (long "debug-mode")
        )
          <*> switch (long "core-mode")
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
  when (dumpParsed opt) $ dump ast
  pure ast

interpret :: MonadIO m => Opt -> Text -> m Value
interpret = M.runMalgo $ do
  desugared <-
    readAndParse
      >>= transWithDump @Rename
      >>= transWithDump @Typing
      >>= transWithDump @Desugar
      >>= trans @LintExp
      >>= transWithDump @Flat
      >>= trans @LintExp
      >>= transWithDump @Optimize
      >>= trans @LintExp
      >>= transWithDump @LambdaLift
  liftIO $ runEval $ evalProgram desugared

compileCore :: MonadIO m => Opt -> Text -> m L.Module
compileCore = M.runMalgo $ do
  opt <- asks maOption
  llvmir <-
    readAndParse
      >>= transWithDump @Rename
      >>= transWithDump @Typing
      >>= transWithDump @Desugar
      >>= trans @LintExp
      >>= transWithDump @Flat
      >>= trans @LintExp
      >>= transWithDump @Optimize
      >>= trans @LintExp
      >>= transWithDump @LambdaLift
      >>= trans @CodeGen
  pure $
    L.defaultModule
      { L.moduleName = fromString $ srcName opt,
        L.moduleSourceFileName = B.toShort $ T.encodeUtf8 $ T.pack $ srcName opt,
        L.moduleDefinitions = llvmir
      }

compile :: MonadIO m => Opt -> Text -> m L.Module
compile = M.runMalgo $ do
  opt <- asks maOption
  llvmir <-
    readAndParse
      >>= transWithDump @Rename
      >>= transWithDump @Typing
      >>= transWithDump @TransToHIR
      >>= transWithDump @HIRLint
      >>= transWithDump @TransToMIR
      >>= transWithDump @MIRLint
      >>= transWithDump @GenLIR
      >>= trans @GenLLVM
  pure $
    L.defaultModule
      { L.moduleName = fromString $ srcName opt,
        L.moduleSourceFileName = B.toShort $ T.encodeUtf8 $ T.pack $ srcName opt,
        L.moduleDefinitions = llvmir
      }

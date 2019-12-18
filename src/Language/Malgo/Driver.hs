{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Language.Malgo.Driver (parseOpt, compile) where

import Language.Malgo.Pretty
import           Language.Malgo.BackEnd.GenLIR
import           Language.Malgo.BackEnd.LLVM
import           Language.Malgo.FrontEnd.Rename
import           Language.Malgo.FrontEnd.Typing.Infer
import qualified Language.Malgo.IR.Syntax             as Syntax
import           Language.Malgo.MiddleEnd.Closure
import           Language.Malgo.MiddleEnd.HIRLint
import           Language.Malgo.MiddleEnd.MIRLint
import           Language.Malgo.MiddleEnd.TransToHIR
import           Language.Malgo.Monad                 as M
import           Language.Malgo.Pass
import qualified LLVM.AST                             as L
import           Options.Applicative
import           Relude

parseOpt :: IO Opt
parseOpt = execParser $
  info ((Opt
          <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")
          <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> value "out.ll" <> help "Write LLVM IR to OUTPUT")
          <*> switch (long "dump-parsed")
          <*> switch (long "dump-renamed")
          <*> switch (long "dump-typed")
          <*> switch (long "dump-knormal")
          <*> switch (long "dump-type-table")
          <*> switch (long "dump-mutrec")
          <*> switch (long "dump-closure"))
          <*> switch (long "debug-mode")
         <**> helper) (fullDesc
    <> progDesc "malgo"
    <> header "malgo - a toy programming language")

compile :: MonadIO m => String -> Syntax.Expr Text -> UniqSupply -> Opt -> m L.Module
compile filename ast = M.runMalgo $ do
  opt <- asks maOption
  when (dumpParsed opt) $
    dump ast
  mir <- transWithDump @Rename ast
    >>= transWithDump @Typing
    >>= transWithDump @TransToHIR
    >>= transWithDump @HIRLint
    >>= transWithDump @Closure
    >>= transWithDump @MIRLint
  lir <- trans @GenLIR mir
  liftIO $ dumpIO lir
  llvmir <- trans @GenLLVM mir
  return $ L.defaultModule { L.moduleName = fromString filename
                           , L.moduleSourceFileName = fromString $ toString filename
                           , L.moduleDefinitions = llvmir
                           }

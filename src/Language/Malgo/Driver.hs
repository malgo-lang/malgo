{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Language.Malgo.Driver where

import qualified Language.Malgo.BackEnd.LLVM             as LLVM
import qualified Language.Malgo.BackEnd.New.LLVM         as New
import           Language.Malgo.FrontEnd.Rename
import           Language.Malgo.FrontEnd.Typing.Infer
import           Language.Malgo.ID
import qualified Language.Malgo.IR.IR                    as IR
import qualified Language.Malgo.IR.Syntax                as Syntax
import           Language.Malgo.MiddleEnd.BasicLint
import           Language.Malgo.MiddleEnd.Closure
import           Language.Malgo.MiddleEnd.MutRec
import qualified Language.Malgo.MiddleEnd.New.Closure    as New
import           Language.Malgo.MiddleEnd.New.HIRLint
import           Language.Malgo.MiddleEnd.New.LIRLint
import           Language.Malgo.MiddleEnd.New.TransToHIR
import           Language.Malgo.MiddleEnd.TransToIR
import           Language.Malgo.Monad                    as M
import           Language.Malgo.Pass
import           Language.Malgo.TypeRep.MType
import qualified LLVM.AST                                as L
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

frontend :: Syntax.Expr Text -> MalgoM (Syntax.Expr TypedID)
frontend ast = do
  opt <- asks maOption
  when (dumpParsed opt) $
    dump ast
  ast' <- transWithDump @Rename ast
  transWithDump @Typing ast'

middleend :: Syntax.Expr TypedID -> MalgoM (IR.Program (ID MType))
middleend ast = transWithDump @TransToIR ast
  >>= transWithDump @BasicLint
  >>= transWithDump @MutRec
  >>= transWithDump @BasicLint
  >>= transWithDump @MutRecLint
  >>= transWithDump @Closure
  >>= transWithDump @BasicProgramLint

backend :: MonadIO m => String -> IR.Program (ID MType) -> m L.Module
backend filename ir = do
  defs <- LLVM.dumpLLVM (LLVM.genProgram ir)
  return $ L.defaultModule { L.moduleName = fromString filename
                           , L.moduleSourceFileName = fromString $ toString filename
                           , L.moduleDefinitions = defs
                           }

compile :: MonadIO m => String -> Syntax.Expr Text -> UniqSupply -> Opt -> m L.Module
compile filename ast = M.runMalgo $ do
  typed <- frontend ast
  hir <- transWithDump @TransToHIR typed
  _ <- transWithDump @HIRLint hir
  lir <- transWithDump @New.Closure hir
  _ <- transWithDump @LIRLint lir
  llvmir <- trans @New.GenLLVM lir
  return $ L.defaultModule { L.moduleName = fromString filename
                           , L.moduleSourceFileName = fromString $ toString filename
                           , L.moduleDefinitions = llvmir
                           }
  -- ir <- middleend typed
  -- backend filename ir

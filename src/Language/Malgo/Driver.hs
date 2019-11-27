{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Language.Malgo.Driver where

import qualified Language.Malgo.BackEnd.LLVM        as LLVM
import           Language.Malgo.FrontEnd.Rename
import           Language.Malgo.FrontEnd.TypeCheck
import           Language.Malgo.ID
import qualified Language.Malgo.IR.IR               as IR
import qualified Language.Malgo.IR.Syntax           as Syntax
import           Language.Malgo.MiddleEnd.BasicLint
import           Language.Malgo.MiddleEnd.Closure
import           Language.Malgo.MiddleEnd.MutRec
import           Language.Malgo.MiddleEnd.TransToIR
import           Language.Malgo.Monad               as M
import           Language.Malgo.Pass
import qualified LLVM.AST                           as L
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
  transform @Rename ast >>= transform @TypeCheck

middleend :: Syntax.Expr TypedID -> MalgoM (IR.Program (ID IR.MType))
middleend ast = transform @TransToIR ast
  >>= transform @BasicLint
  >>= transform @MutRec
  >>= transform @BasicLint
  >>= transform @MutRecLint
  >>= transform @Closure
  >>= transform @BasicProgramLint

backend :: MonadIO m => String -> IR.Program (ID IR.MType) -> m L.Module
backend filename ir = do
  defs <- LLVM.dumpLLVM (LLVM.genProgram ir)
  return $ L.defaultModule { L.moduleName = fromString filename
                           , L.moduleSourceFileName = fromString $ toString filename
                           , L.moduleDefinitions = defs
                           }

compile :: MonadIO m => String -> Syntax.Expr Text -> UniqSupply -> Opt -> m L.Module
compile filename ast = M.runMalgo $ do
  typed <- frontend ast
  ir <- middleend typed
  backend filename ir

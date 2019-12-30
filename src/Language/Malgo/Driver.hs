{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Language.Malgo.Driver
  ( parseOpt
  , compile
  )
where

import           Language.Malgo.BackEnd.GenLIR
import           Language.Malgo.BackEnd.GenLLVM
import           Language.Malgo.FrontEnd.Rename
import           Language.Malgo.FrontEnd.Typing.Infer
import qualified Language.Malgo.IR.Syntax      as Syntax
import           Language.Malgo.MiddleEnd.Closure
-- import           Language.Malgo.MiddleEnd.HIRLint
-- import           Language.Malgo.MiddleEnd.MIRLint
import           Language.Malgo.MiddleEnd.TransToHIR
import           Language.Malgo.Monad          as M
import           Language.Malgo.Pass
import qualified LLVM.AST                      as L
import           Options.Applicative
import           Language.Malgo.Prelude

parseOpt :: IO Opt
parseOpt = execParser $ info
  (    (   Opt
       <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")
       <*> strOption
             (long "output" <> short 'o' <> metavar "OUTPUT" <> value "out.ll" <> help
               "Write LLVM IR to OUTPUT"
             )
       <*> switch (long "dump-parsed")
       <*> switch (long "dump-renamed")
       <*> switch (long "dump-typed")
       <*> switch (long "dump-knormal" <> long "dump-hir")
       <*> switch (long "dump-type-table")
       <*> switch (long "dump-mutrec")
       <*> switch (long "dump-closure" <> long "dump-mir")
       <*> switch (long "dump-lir")
       )
  <*>  switch (long "debug-mode")
  <**> helper
  )
  (fullDesc <> progDesc "malgo" <> header "malgo - a toy programming language")

compile :: MonadIO m => String -> Syntax.Expr Text -> UniqSupply -> Opt -> m L.Module
compile filename ast = M.runMalgo $ do
  opt <- asks maOption
  when (dumpParsed opt) $ dump ast
  mir <-
    transWithDump @Rename ast
    >>= transWithDump @Typing
    >>= transWithDump @TransToHIR
    -- >>= transWithDump @HIRLint
    >>= transWithDump @Closure
    -- >>= transWithDump @MIRLint
  lir    <- transWithDump @GenLIR mir
  llvmir <- trans @GenLLVM lir
  return $ L.defaultModule { L.moduleName           = fromString filename
                           , L.moduleSourceFileName = encodeUtf8 filename
                           , L.moduleDefinitions    = llvmir
                           }

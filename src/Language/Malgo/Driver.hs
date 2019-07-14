{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.Driver where

import           Data.Outputable
import qualified Language.Malgo.BackEnd.LLVM        as LLVM
import qualified Language.Malgo.FrontEnd.Rename     as Rename
import qualified Language.Malgo.FrontEnd.TypeCheck  as TypeCheck
import           Language.Malgo.ID
import qualified Language.Malgo.IR.IR               as IR
import qualified Language.Malgo.IR.Syntax           as Syntax
import qualified Language.Malgo.MiddleEnd.BasicLint as BasicLint
import qualified Language.Malgo.MiddleEnd.Closure   as Closure
import qualified Language.Malgo.MiddleEnd.MutRec    as MutRec
import qualified Language.Malgo.MiddleEnd.TransToIR as TransToIR
import           Language.Malgo.Monad               as M
import           Language.Malgo.Pretty
import qualified LLVM.AST                               as L
import           Options.Applicative
import           Universum

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
          <*> switch (long "dump-closure"))
          <*> switch (long "debug-mode")
         <**> helper) (fullDesc
    <> progDesc "malgo"
    <> header "malgo - a toy programming language")

dump :: (MonadReader MalgoEnv m, Outputable a, MonadIO m, Pretty a) => a -> m ()
dump x = do
  opt <- asks maOption
  if isDebugMode opt
  then print $ ppr x
  else print $ pPrint x

frontend :: Syntax.Expr Text -> MalgoM (Syntax.Expr TypedID)
frontend ast = do
  opt <- asks maOption
  when (dumpParsed opt) $
    dump ast
  renamed <- Rename.rename ast
  when (dumpRenamed opt) $
    dump renamed
  typed <- TypeCheck.typeCheck renamed
  when (dumpTyped opt) $
    dump typed
  return typed

middleend :: Syntax.Expr TypedID -> MalgoM (IR.Program (ID IR.MType))
middleend ast = do
  opt <- asks maOption
  ir <- TransToIR.trans ast
  when (dumpKNormal opt) $
    dump ir
  case BasicLint.lint ir of
    Right _  -> pass
    Left mes -> error $ show mes
  M.UniqSupply u <- M.maUniqSupply <$> ask
  writeIORef u 0
  ir' <- MutRec.remove ir
  case BasicLint.lint ir' of
    Right _  -> pass
    Left mes -> error $ show mes
  case MutRec.lint ir' of
    Right _  -> pass
    Left mes -> error $ show mes

  ir'' <- Closure.trans ir'
  when (dumpClosure opt) $
    dump ir''
  case BasicLint.runLint (BasicLint.lintProgram ir'') of
    Right _  -> pass
    Left mes -> error $ show mes

  return ir''

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

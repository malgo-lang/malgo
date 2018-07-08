{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Driver where

import           Data.Text.Prettyprint.Doc
import qualified Language.Malgo.BackEnd.LLVM        as LLVM
import qualified Language.Malgo.FrontEnd.Rename     as Rename
import qualified Language.Malgo.FrontEnd.TypeCheck  as TypeCheck
import           Language.Malgo.ID
import qualified Language.Malgo.IR.IR               as IR
import qualified Language.Malgo.IR.Syntax           as Syntax
import qualified Language.Malgo.MiddleEnd.BasicLint as BasicLint
import qualified Language.Malgo.MiddleEnd.Closure   as Closure'
import qualified Language.Malgo.MiddleEnd.MutRec    as MutRec
import qualified Language.Malgo.MiddleEnd.TransToIR as TransToIR
import qualified Language.Malgo.Monad               as M
import qualified LLVM.AST                           as L
import           Options.Applicative
import           RIO
import qualified RIO.Text                           as Text

data Opt = Opt
  { _srcName         :: Text
  , _dumpParsed      :: Bool
  , _dumpRenamed     :: Bool
  , _dumpTyped       :: Bool
  , _dumpHIR         :: Bool
  , _dumpFlatten     :: Bool
  , _dumpClosure     :: Bool
  , _dumpIR          :: Bool
  , _notDeleteUnused :: Bool
  , _notBetaTrans    :: Bool
  } deriving (Eq, Show)

parseOpt :: IO Opt
parseOpt = execParser $
  info ((Opt
          <$> strArgument (metavar "(FILENAME)" <> help "Source file" <> action "file")
          <*> switch (long "dump-parsed")
          <*> switch (long "dump-renamed")
          <*> switch (long "dump-typed")
          <*> switch (long "dump-hir")
          <*> switch (long "dump-flatten")
          <*> switch (long "dump-closure"))
          <*> switch (long "dump-ir")
          <*> switch (long "not-delete-unused")
          <*> switch (long "not-beta-trans")
         <**> helper)
  (fullDesc
    <> progDesc "malgo"
    <> header "malgo - a toy programming language")

frontend :: Syntax.Expr Text -> Opt -> RIO M.MalgoApp (Syntax.Expr TypedID)
frontend ast opt = do
  when (_dumpParsed opt) $
    logInfo $ displayShow $ pretty ast
  renamed <- Rename.rename ast
  when (_dumpRenamed opt) $
    logInfo $ displayShow $ pretty renamed
  typed <- TypeCheck.typeCheck renamed
  when (_dumpTyped opt) $
    logInfo $ displayShow $ pretty typed
  return typed

middleend :: Syntax.Expr TypedID -> Opt -> RIO M.MalgoApp (IR.Program (ID IR.MType))
middleend ast opt = do
  ir <- TransToIR.trans ast
  when (_dumpIR opt) $ do
    logInfo "TransToIR:"
    logInfo $ displayShow $ pretty $ IR.flattenExpr ir
  case BasicLint.lint ir of
    Right _  -> return ()
    Left mes -> error $ show mes
  M.UniqSupply u <- M.maUniqSupply <$> ask
  writeIORef u 0
  ir' <- MutRec.remove ir
  when (_dumpIR opt) $ do
    logInfo "MutRec:"
    logInfo $ displayShow $ pretty $ IR.flattenExpr ir'
  case BasicLint.lint ir' of
    Right _  -> return ()
    Left mes -> error $ show mes
  case MutRec.lint ir' of
    Right _  -> return ()
    Left mes -> error $ show mes

  ir'' <- Closure'.trans ir'
  when (_dumpIR opt && _dumpClosure opt) $ do
    logInfo "Closure:"
    logInfo $ displayShow $ pretty $ IR.flattenProgram ir''
  case BasicLint.runLint (BasicLint.lintProgram ir'') of
    Right _  -> return ()
    Left mes -> error $ show mes

  return ir''

backend :: MonadIO m => Text -> IR.Program (ID IR.MType) -> m L.Module
backend filename ir = do
  defs <- LLVM.dumpLLVM (LLVM.genProgram ir)
  return $ L.defaultModule { L.moduleName = fromString $ Text.unpack filename
                           , L.moduleSourceFileName = fromString $ Text.unpack filename
                           , L.moduleDefinitions = defs
                           }

compile :: MonadIO m => Text -> Syntax.Expr Text -> Opt -> M.UniqSupply -> m L.Module
compile filename ast opt = M.runMalgo $ do
  typed <- frontend ast opt
  ir <- middleend typed opt
  backend filename ir

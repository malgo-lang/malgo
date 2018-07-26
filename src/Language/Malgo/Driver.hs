{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Driver where

import           Language.Malgo.Pretty
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
  { _srcName     :: Text
  , _dumpParsed  :: Bool
  , _dumpRenamed :: Bool
  , _dumpTyped   :: Bool
  , _dumpKNormal :: Bool
  -- , _dumpMutRec      :: Bool
  , _dumpClosure :: Bool
  -- , _notDeleteUnused :: Bool
  -- , _notBetaTrans    :: Bool
  } deriving (Eq, Show)

parseOpt :: IO Opt
parseOpt = execParser $
  info ((Opt
          <$> strArgument (metavar "(FILENAME)" <> help "Source file" <> action "file")
          <*> switch (long "dump-parsed")
          <*> switch (long "dump-renamed")
          <*> switch (long "dump-typed")
          <*> switch (long "dump-knormal")
          <*> switch (long "dump-closure"))
          -- <*> switch (long "not-delete-unused")
          -- <*> switch (long "not-beta-trans")
         <**> helper)
  (fullDesc
    <> progDesc "malgo"
    <> header "malgo - a toy programming language")

frontend :: Syntax.Expr Text -> Opt -> RIO M.MalgoApp (Syntax.Expr TypedID)
frontend ast opt = do
  when (_dumpParsed opt) $
    logInfo $ displayShow $ pPrint ast
  renamed <- Rename.rename ast
  when (_dumpRenamed opt) $
    logInfo $ displayShow $ pPrint renamed
  typed <- TypeCheck.typeCheck renamed
  when (_dumpTyped opt) $
    logInfo $ displayShow $ pPrint typed
  return typed

middleend :: Syntax.Expr TypedID -> Opt -> RIO M.MalgoApp (IR.Program (ID IR.MType))
middleend ast opt = do
  ir <- TransToIR.trans ast
  when (_dumpKNormal opt) $
    logInfo $ displayShow $ pPrint $ IR.flattenExpr ir
  case BasicLint.lint ir of
    Right _  -> return ()
    Left mes -> error $ show mes
  M.UniqSupply u <- M.maUniqSupply <$> ask
  writeIORef u 0
  ir' <- MutRec.remove ir
  -- when (_dumpIR opt) $ do
  --   logInfo "MutRec:"
  --   logInfo $ displayShow $ pPrint $ IR.flattenExpr ir'
  case BasicLint.lint ir' of
    Right _  -> return ()
    Left mes -> error $ show mes
  case MutRec.lint ir' of
    Right _  -> return ()
    Left mes -> error $ show mes

  ir'' <- Closure'.trans ir'
  when (_dumpClosure opt) $
    logInfo $ displayShow $ pPrint $ IR.flattenProgram ir''
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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Driver where

import           Data.Outputable
import qualified Language.Malgo.BackEnd.LLVM                 as LLVM
import qualified Language.Malgo.FrontEnd.Rename              as Rename
import qualified Language.Malgo.FrontEnd.TypeCheck           as TypeCheck
import           Language.Malgo.ID
import qualified Language.Malgo.IR.IR                        as IR
import qualified Language.Malgo.IR.Syntax                    as Syntax
import qualified Language.Malgo.MiddleEnd.BasicLint          as BasicLint
import qualified Language.Malgo.MiddleEnd.Closure.Preprocess as Closure
import qualified Language.Malgo.MiddleEnd.Closure.Trans      as Closure
import qualified Language.Malgo.MiddleEnd.MutRec             as MutRec
import qualified Language.Malgo.MiddleEnd.TransToIR          as TransToIR
import qualified Language.Malgo.Monad                        as M
import           Language.Malgo.Pretty
import qualified LLVM.AST                                    as L
import           Options.Applicative
import           RIO
import qualified RIO.Map                                     as Map
import qualified RIO.Text                                    as Text

data Opt = Opt
  { _srcName       :: Text
  , _dumpParsed    :: Bool
  , _dumpRenamed   :: Bool
  , _dumpTyped     :: Bool
  , _dumpKNormal   :: Bool
  , _dumpTypeTable :: Bool
  -- , _dumpMutRec      :: Bool
  , _dumpClosure   :: Bool
  , _isDebugMode :: Bool
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
          <*> switch (long "dump-type-table")
          <*> switch (long "dump-closure"))
          <*> switch (long "debug-mode")
          -- <*> switch (long "not-delete-unused")
          -- <*> switch (long "not-beta-trans")
         <**> helper)
  (fullDesc
    <> progDesc "malgo"
    <> header "malgo - a toy programming language")

dump :: (MonadReader env m, Outputable a, MonadIO m, HasLogFunc env, Pretty a) => Opt -> a -> m ()
dump opt x =
  if _isDebugMode opt
  then logInfo $ displayShow $ ppr x
  else logInfo $ displayShow $ pPrint x

frontend :: Syntax.Expr Text -> Opt -> RIO M.MalgoApp (Syntax.Expr TypedID)
frontend ast opt = do
  when (_dumpParsed opt) $
    dump opt ast
  renamed <- Rename.rename ast
  when (_dumpRenamed opt) $
    dump opt renamed
  typed <- TypeCheck.typeCheck renamed
  when (_dumpTyped opt) $
    dump opt typed
  return typed

middleend :: Syntax.Expr TypedID -> Opt -> RIO M.MalgoApp (IR.Program (ID IR.MType))
middleend ast opt = do
  ir <- TransToIR.trans ast
  when (_dumpKNormal opt) $
    dump opt $ IR.flattenExpr ir
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

  let (_, tt) = Closure.divideTypeFromExpr ir'
  when (_dumpTypeTable opt) $
    logInfo $ displayShow $ ppr $ Map.toList tt

  ir'' <- Closure.trans ir'
  when (_dumpClosure opt) $
    dump opt $ IR.flattenProgram ir''
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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Driver where

import           Control.Lens                       (view)
import qualified Language.Malgo.Beta                as Beta
import qualified Language.Malgo.Closure             as Closure
import qualified Language.Malgo.CodeGen             as CodeGen
import qualified Language.Malgo.Flatten             as Flatten
import qualified Language.Malgo.FrontEnd.Rename     as Rename
import qualified Language.Malgo.FrontEnd.TypeCheck  as TypeCheck
import           Language.Malgo.ID
import qualified Language.Malgo.IR.IR               as IR
import qualified Language.Malgo.IR.Syntax           as Syntax
import qualified Language.Malgo.KNormal             as KNormal
import qualified Language.Malgo.MiddleEnd.BasicLint as BasicLint
import qualified Language.Malgo.MiddleEnd.TransToIR as TransToIR
import qualified Language.Malgo.MiddleEnd.MutRec as MutRec
import qualified Language.Malgo.Monad               as M
import           Language.Malgo.Prelude
import qualified Language.Malgo.Unused              as Unused
import qualified LLVM.AST                           as L
import           Options.Applicative
import           RIO                                (RIO, newIORef, readIORef)

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

frontend :: Syntax.Expr Text -> Opt -> RIO M.MalgoApp (Syntax.Expr TypedID, Int)
frontend ast opt = do
  when (_dumpParsed opt) $
    print $ pretty ast
  renamed <- Rename.rename ast
  when (_dumpRenamed opt) $
    print $ pretty renamed
  typed <- TypeCheck.typeCheck renamed
  when (_dumpTyped opt) $
    print $ pretty typed
  M.UniqSupply u <- M.maUniqSupply <$> ask
  i <- readIORef u
  return (typed, i)

middleend :: Syntax.Expr TypedID -> Opt -> RIO M.MalgoApp (IR.Expr (ID IR.MType), Int)
middleend ast opt = do
  ir <- TransToIR.trans ast
  case BasicLint.lint ir of
    Right _  -> return ()
    Left mes -> error $ show mes
  when (_dumpIR opt) $
    print $ pretty ir

  ir' <- MutRec.remove ir
  case BasicLint.lint ir' of
    Right _  -> return ()
    Left mes -> error $ show mes
  case MutRec.lint ir' of
    Right _ -> return ()
    Left mes -> error $ show mes
  when (_dumpIR opt) $
    print $ pretty ir'
  M.UniqSupply u <- M.maUniqSupply <$> ask
  i <- readIORef u
  return (ir', i)

compile :: Text -> Syntax.Expr Name -> Opt -> IO L.Module
compile filename ast opt = do
  i <- newIORef 0
  (typed, s2) <- M.runMalgo' (frontend ast opt) (M.UniqSupply i)
  (knormal, s3) <- run _dumpHIR (KNormal.knormal $
                                  if _notBetaTrans opt
                                  then typed
                                  else Beta.betaTrans typed) s2
  when (_dumpFlatten opt) $
    liftIO (print $ pretty (Flatten.flatten knormal))
  (cls, _) <- run (const False) (Closure.conv knormal) s3
  when (_dumpClosure opt) $
    liftIO $ print $ pretty cls
  let defs = CodeGen.dumpCodeGen (CodeGen.genProgram $
                                  if _notDeleteUnused opt
                                  then cls
                                  else Unused.remove cls)
  let llvmMod = L.defaultModule { L.moduleName = fromString $ toS filename
                                , L.moduleSourceFileName = fromString $ toS filename
                                , L.moduleDefinitions = defs
                                }
  return llvmMod
  where run key m u = do
          (x, s) <- M.runMalgo m u
          when (key opt) $
            liftIO $ print $ pretty x
          s' <- readIORef $ M.unUniqSupply $ view M.uniqSupplyL s
          return (x, s')

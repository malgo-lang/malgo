{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Driver where

import qualified Language.Malgo.Beta      as Beta
import qualified Language.Malgo.Closure   as Closure
import qualified Language.Malgo.CodeGen   as CodeGen
import qualified Language.Malgo.Flatten   as Flatten
import qualified Language.Malgo.KNormal   as KNormal
import qualified Language.Malgo.Monad     as M
import           Language.Malgo.Prelude
import qualified Language.Malgo.Rename    as Rename
import qualified Language.Malgo.Syntax    as Syntax
import qualified Language.Malgo.TypeCheck as TypeCheck
import qualified Language.Malgo.Unused    as Unused

import           Control.Lens             (view)
import qualified LLVM.AST                 as L
import           Options.Applicative

data Opt = Opt
  { _srcName         :: Text
  , _dumpParsed      :: Bool
  , _dumpRenamed     :: Bool
  , _dumpTyped       :: Bool
  , _dumpHIR         :: Bool
  , _dumpFlatten     :: Bool
  , _dumpClosure     :: Bool
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
          <*> switch (long "not-delete-unused")
          <*> switch (long "not-beta-trans")
         <**> helper)
  (fullDesc
    <> progDesc "malgo"
    <> header "malgo - a toy programming language")

compile :: Text -> Syntax.Expr Name -> Opt -> IO L.Module
compile filename ast opt = do
  when (_dumpParsed opt) $
    liftIO . print $ ppr ast
  (renamed, s1) <- run' _dumpRenamed (Rename.rename ast) 0
  (typed, s2) <- run' _dumpTyped (TypeCheck.typeCheck renamed) s1
  (knormal, s3) <- run' _dumpHIR (KNormal.knormal $
                                  if _notBetaTrans opt
                                  then typed
                                  else Beta.betaTrans typed) s2
  when (_dumpFlatten opt) $
    liftIO (print $ ppr (Flatten.flatten knormal))
  (cls, _) <- run (const False) (Closure.conv knormal) s3
  when (_dumpClosure opt) $
    liftIO $ print $ ppr cls
  let defs = CodeGen.dumpCodeGen (CodeGen.genProgram $
                                  if _notDeleteUnused opt
                                  then cls
                                  else Unused.remove cls)
  let llvmMod = L.defaultModule { L.moduleName = fromString $ toS filename
                                , L.moduleSourceFileName = fromString $ toS filename
                                , L.moduleDefinitions = defs
                                }
  pure llvmMod
  where run key m u = do
          (x, s) <- M.runMalgo m u
          when (key opt) $
            liftIO $ print $ ppr x
          s' <- M.readMutVar $ M.unUniqSupply $ view M.uniqSupplyL s
          return (x, s')
        run' key m u = do
          (x, s) <- M.runMalgo' m u
          when (key opt) $
            liftIO $ print $ ppr x
          s' <- M.readMutVar $ M.unUniqSupply $ view M.uniqSupplyL s
          return (x, s')

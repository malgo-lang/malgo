{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Language.Malgo.Driver where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Semigroup
import qualified Language.Malgo.Beta      as Beta
import qualified Language.Malgo.Closure   as Closure
import qualified Language.Malgo.Eval      as Eval
import qualified Language.Malgo.Flatten   as Flatten
import qualified Language.Malgo.HIR       as HIR
import qualified Language.Malgo.KNormal   as KNormal
import           Language.Malgo.MIR
import qualified Language.Malgo.Rename    as Rename
import qualified Language.Malgo.Syntax    as Syntax
import qualified Language.Malgo.TypeCheck as TypeCheck
import qualified Language.Malgo.Unused    as Unused
import           Language.Malgo.Utils
import           Options.Applicative
import           System.IO

data Opt = Opt { _srcName         :: String
               , _dumpParsed      :: Bool
               , _dumpRenamed     :: Bool
               , _dumpTyped       :: Bool
               , _dumpHIR         :: Bool
               , _dumpBeta        :: Bool
               , _dumpFlatten     :: Bool
               , _dumpClosure     :: Bool
               , _compileOnly     :: Bool
               , _notRemoveUnused :: Bool
               }
  deriving (Eq, Show)

parseOpt :: IO Opt
parseOpt = execParser $
  info ((Opt
          <$> strArgument (metavar "(FILENAME)" <> help "Source file" <> action "file")
          <*> switch (long "dump-parsed")
          <*> switch (long "dump-renamed")
          <*> switch (long "dump-typed")
          <*> switch (long "dump-hir")
          <*> switch (long "dump-beta")
          <*> switch (long "dump-flatten")
          <*> switch (long "dump-closure"))
          <*> switch (long "compile-only")
          <*> switch (long "not-remove-unused")
         <**> helper)
  (fullDesc
    <> progDesc "A interpreter of malgo"
    <> header "malgo - a toy programming language")

type Obj = StateT Int IO (Program TypeCheck.TypedID)

compile :: Syntax.Expr Name -> Opt -> Obj
compile ast opt = do
  when (_dumpParsed opt) $
    lift . print $ pretty ast
  renamed <- run _dumpRenamed Rename.rename ast
  typed <- run _dumpTyped TypeCheck.typeCheck renamed
  knormal <- run _dumpHIR KNormal.knormal typed
  beta <- run _dumpBeta Beta.betaTrans knormal
  flat <- run _dumpFlatten flattenM beta
  cls <- run (const False) Closure.conv flat
  let cls' = if _notRemoveUnused opt
             then cls
             else Unused.remove cls
  when (_dumpClosure opt) $
    lift . hPrint stderr $ pretty cls'
  return cls'
  where run key f x =
          doMalgoT (f x) >>= \case
          Left x' -> error $ show x'
          Right x' -> do when (key opt) $
                           lift . hPrint stderr $ pretty x'
                         return x'
        flattenM :: HIR.Expr TypeCheck.TypedID -> Beta.Beta IO (HIR.Expr TypeCheck.TypedID)
        flattenM = return . Flatten.flatten

eval :: Obj -> IO (Either MalgoError Eval.Value)
eval m = flip evalStateT 0 $ do
  prog <- m
  doMalgoT $ Eval.eval prog

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Prelude
  ( module Koriel.Prelude,
    MonadMalgo (..),
    MalgoM (..),
    errorOn,
    Opt (..),
    parseOpt,
    getPackagePathes,
    getPackagePath
  )
where

import Control.Monad.Fix (MonadFix)
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import Options.Applicative
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath ((</>), takeDirectory)
import System.FilePath.Lens
import Text.Megaparsec.Pos (SourcePos (sourceLine), unPos)

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT Opt IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadFail)

class Monad m => MonadMalgo m where
  getOpt :: m Opt
  default getOpt :: (MonadTrans t, MonadMalgo m1, m ~ t m1) => m Opt
  getOpt = lift getOpt

viewLine :: (MonadMalgo m, MonadIO m) => Int -> m String
viewLine linum = do
  srcFileName <- srcName <$> getOpt
  s <- liftIO $ readFile srcFileName
  pure $ lines s !! (linum - 1)

errorOn :: (HasCallStack, MonadMalgo m, MonadIO m) => SourcePos -> Doc -> m a
errorOn pos x = do
  line <- viewLine (unPos $ sourceLine pos)
  errorDoc $
    "error on" <+> pPrint pos <> ":" $+$ nest 2 x
      $+$ pPrint (unPos $ sourceLine pos) <+> text line

instance MonadMalgo MalgoM where
  getOpt = MalgoM ask

instance MonadMalgo m => MonadMalgo (ReaderT r m)

instance MonadMalgo m => MonadMalgo (ExceptT e m)

instance MonadMalgo m => MonadMalgo (StateT s m)

instance MonadMalgo m => MonadMalgo (WriterT w m)

instance MonadMalgo m => MonadMalgo (UniqT m)

data Opt = Opt
  { srcName :: FilePath,
    dstName :: FilePath,
    dumpParsed :: Bool,
    dumpRenamed :: Bool,
    dumpTyped :: Bool,
    dumpDesugar :: Bool,
    genCoreJSON :: Bool,
    noOptimize :: Bool,
    noLambdaLift :: Bool,
    inlineSize :: Int,
    viaBinding :: Bool,
    debugMode :: Bool,
    modulePaths :: [FilePath]
  }
  deriving stock (Eq, Show)

parseOpt :: IO Opt
parseOpt = do
  opt <-
    execParser $
      info
        ( ( Opt
              <$> strArgument (metavar "SOURCE" <> help "Source file" <> action "file")
              <*> strOption
                ( long "output" <> short 'o' <> metavar "OUTPUT" <> value ""
                    <> help
                      "Write LLVM IR to OUTPUT"
                )
              <*> switch (long "dump-parsed")
              <*> switch (long "dump-renamed")
              <*> switch (long "dump-typed")
              <*> switch (long "dump-desugar")
              <*> switch (long "gen-core-json")
              <*> switch (long "no-lambdalift")
              <*> switch (long "no-opt")
              <*> fmap read (strOption (long "inline" <> value "10"))
              <*> switch (long "via-binding")
              <*> switch (long "debug-mode")
              <*> many (strOption (long "module-path" <> short 'M' <> metavar "MODULE_PATH"))
          )
            <**> helper
        )
        (fullDesc <> progDesc "griff" <> header "griff - a programming language")
  if null (dstName opt)
    then pure opt {dstName = srcName opt & extension .~ ".ll"}
    else pure opt

getPackagePathes :: (MonadMalgo m, MonadIO m) => m [FilePath]
getPackagePathes = do
  opt <- getOpt
  basePath <- getPackagePath "base"
  pure $ takeDirectory (dstName opt) : modulePaths opt <> [basePath]

getPackagePath :: MonadIO m => FilePath -> m FilePath
getPackagePath packageName =
  liftIO $ getXdgDirectory XdgData ("malgo" </> packageName)
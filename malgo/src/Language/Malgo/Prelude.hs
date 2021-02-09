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
    defaultOpt,
    getPackagePathes,
    getPackagePath,
  )
where

import Control.Monad.Fix (MonadFix)
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath (takeDirectory, (</>), (-<.>))
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

defaultOpt :: FilePath -> Opt
defaultOpt src =
  Opt
    { srcName = src,
      dstName = src -<.> "ll",
      dumpParsed = False,
      dumpRenamed = False,
      dumpTyped = False,
      dumpDesugar = False,
      genCoreJSON = False,
      noOptimize = False,
      noLambdaLift = False,
      inlineSize = 10,
      viaBinding = False,
      debugMode = False,
      modulePaths = []
    }

getPackagePathes :: (MonadMalgo m, MonadIO m) => m [FilePath]
getPackagePathes = do
  opt <- getOpt
  basePath <- getPackagePath "base"
  pure $ takeDirectory (dstName opt) : modulePaths opt <> [basePath]

getPackagePath :: MonadIO m => FilePath -> m FilePath
getPackagePath packageName =
  liftIO $ getXdgDirectory XdgData ("malgo" </> packageName)
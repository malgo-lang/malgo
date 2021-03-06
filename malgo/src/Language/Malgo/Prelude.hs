{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Prelude
  ( module Koriel.Prelude,
    MonadMalgo (..),
    MalgoM (..),
    errorOn,
    Opt (..),
    defaultOpt,
    With (..),
    ann,
    value,
  )
where

import Control.Monad.Fix (MonadFix)
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import System.FilePath ((-<.>))
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

#ifdef DEBUG
errorOn :: (HasCallStack, MonadMalgo m, MonadIO m) => SourcePos -> Doc -> m a
#else
errorOn :: (MonadMalgo m, MonadIO m) => SourcePos -> Doc -> m a
#endif
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
    modulePaths :: [FilePath],
    forceRebuild :: Bool
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
      modulePaths = [],
      forceRebuild = False
    }

data With x v = With {_ann :: x, _value :: v}
  deriving stock (Eq, Ord, Bounded, Read, Show, Generic)

makeLenses ''With

instance (Pretty x, Pretty v) => Pretty (With x v) where
  pPrintPrec l _ (With x v) = pPrintPrec l 0 v <> brackets (pPrintPrec l 0 x)

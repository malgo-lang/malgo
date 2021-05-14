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
    MalgoM (..),
    Opt (..),
    HasOpt (..),
    getOpt,
    errorOn,
    defaultOpt,
    With (..),
    ann,
    value,
  )
where

import Control.Monad.Fix (MonadFix)
import Koriel.Prelude
import Koriel.Pretty
import System.FilePath ((-<.>))
import Text.Megaparsec.Pos (SourcePos (sourceLine), unPos)

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT Opt IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadReader Opt, MonadFail)

data Opt = Opt
  { srcName :: FilePath,
    dstName :: FilePath,
    dumpParsed :: Bool,
    dumpRenamed :: Bool,
    dumpTyped :: Bool,
    dumpDesugar :: Bool,
    noOptimize :: Bool,
    noLambdaLift :: Bool,
    inlineSize :: Int,
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
      noOptimize = False,
      noLambdaLift = False,
      inlineSize = 10,
      debugMode = False,
      modulePaths = [],
      forceRebuild = False
    }

class HasOpt env where
  malgoOpt :: Lens' env Opt

instance HasOpt Opt where
  malgoOpt = lens id const

getOpt :: (HasOpt env, MonadReader env m) => m Opt
getOpt = view malgoOpt

viewLine :: (HasOpt env, MonadReader env m, MonadIO m) => Int -> m String
viewLine linum = do
  srcFileName <- srcName <$> getOpt
  s <- liftIO $ readFile srcFileName
  pure $ lines s !! (linum - 1)

#ifdef DEBUG
errorOn :: (HasCallStack, HasOpt env, MonadReader env m, MonadIO m) => SourcePos -> Doc -> m a
#else
errorOn :: (HasOpt env, MonadReader env m, MonadIO m) => SourcePos -> Doc -> m a
#endif
errorOn pos x = do
  line <- viewLine (unPos $ sourceLine pos)
  errorDoc $
    "error on" <+> pPrint pos <> ":" $+$ nest 2 x
      $+$ pPrint (unPos $ sourceLine pos) <+> "|" <+> text line

data With x v = With {_ann :: x, _value :: v}
  deriving stock (Eq, Ord, Bounded, Read, Show, Generic)

makeLenses ''With

instance (Pretty x, Pretty v) => Pretty (With x v) where
  pPrintPrec l _ (With x v) = pPrintPrec l 0 v <> brackets (pPrintPrec l 0 x)

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Monad
  ( MalgoM (..),
    MalgoEnv (..),
    runMalgo,
    MonadMalgo (..),
    Opt (..),
    viewLine,
    malgoError,
  )
where

import Control.Monad.Fix
import qualified Data.Text as T
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import Text.Parsec.Pos
  ( SourcePos,
    sourceLine,
  )

data Opt = Opt
  { srcName :: String,
    dstName :: String,
    dumpParsed :: Bool,
    dumpRenamed :: Bool,
    dumpTyped :: Bool,
    dumpTypeTable :: Bool,
    dumpDesugar :: Bool,
    dumpLambdaLift :: Bool,
    dumpFlat :: Bool,
    isDebugMode :: Bool,
    applyLambdaLift :: Bool,
    noOptimize :: Bool,
    inlineSize :: Int
  }
  deriving stock (Eq, Show)

data MalgoEnv = MalgoEnv
  { maOption :: Opt,
    maSource :: Text
  }

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv (UniqT IO) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadReader MalgoEnv, MonadIO, MonadFix, MonadFail, MonadUniq)

runMalgo :: MonadIO m => MalgoM a -> Opt -> Text -> m a
runMalgo (MalgoM m) opt source =
  liftIO $
    fmap fst $
      runUniqT ?? UniqSupply 0 $
        runReaderT
          m
          MalgoEnv {maOption = opt, maSource = source}

class Monad m => MonadMalgo m where
  getOpt :: m Opt
  default getOpt :: (MonadTrans t, MonadMalgo m1, m ~ t m1) => m Opt
  getOpt = lift getOpt
  getSource :: m Text
  default getSource :: (MonadTrans t, MonadMalgo m1, m ~ t m1) => m Text
  getSource = lift getSource

instance MonadMalgo MalgoM where
  getOpt = asks maOption
  getSource = asks maSource

instance (MonadIO m, MonadMalgo m) => MonadMalgo (ReaderT r m)

instance (MonadIO m, MonadMalgo m) => MonadMalgo (ExceptT e m)

instance (MonadIO m, MonadMalgo m) => MonadMalgo (StateT s m)

instance (MonadIO m, MonadMalgo m) => MonadMalgo (WriterT w m)

viewLine :: MonadMalgo m => Int -> m Text
viewLine linum = do
  s <- getSource
  pure $ T.lines s !! (linum - 1)

malgoError :: MonadMalgo m => SourcePos -> Doc -> Doc -> m a
malgoError pos tag mes = do
  line <- viewLine (sourceLine pos)
  errorDoc $
    "error("
      <> tag
      <> "):"
      <+> mes
      $$ "on"
      <+> pPrint pos
      <> ":"
      $$ text (T.unpack line)
      <> "\n"

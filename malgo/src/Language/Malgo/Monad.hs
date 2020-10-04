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
    getFileName,
    viewLine,
    malgoError,
    MonadUniq (..),
    UniqSupply(..),
    UniqT(..),
    runUniqT,
  )
where

import Control.Monad.Fix
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import LLVM.IRBuilder (IRBuilderT, ModuleBuilderT)
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import System.IO (stderr)
import Text.Parsec.Pos (SourcePos, sourceLine)
import Text.PrettyPrint (text, ($$))

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

newtype UniqSupply = UniqSupply {uniqSupply :: Int}

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv (Lazy.StateT UniqSupply IO) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadReader MalgoEnv, MonadState UniqSupply, MonadIO, MonadFix, MonadFail)

runMalgo :: MonadIO m => MalgoM a -> Opt -> Text -> m a
runMalgo (MalgoM m) opt source =
  liftIO $
    Lazy.evalStateT ?? UniqSupply 0 $
      runReaderT
        m
        MalgoEnv
          { maOption = opt,
            maSource = source
          }

class Monad m => MonadMalgo m where
  getOpt :: m Opt
  default getOpt :: (MonadTrans t, MonadMalgo m1, m ~ t m1) => m Opt
  getOpt = lift getOpt
  getSource :: m Text
  default getSource :: (MonadTrans t, MonadMalgo m1, m ~ t m1) => m Text
  getSource = lift getSource
  printLog :: Text -> m ()
  default printLog :: MonadIO m => Text -> m ()
  printLog = liftIO . T.hPutStrLn stderr

instance MonadMalgo MalgoM where
  getOpt = asks maOption
  getSource = asks maSource
  printLog = liftIO . T.hPutStrLn stderr

instance (MonadIO m, MonadMalgo m) => MonadMalgo (ReaderT r m)

instance (MonadIO m, MonadMalgo m) => MonadMalgo (ExceptT e m)

instance (MonadIO m, MonadMalgo m) => MonadMalgo (StateT s m)

instance (MonadIO m, MonadMalgo m) => MonadMalgo (WriterT w m)

getFileName :: (MonadMalgo m, IsString a) => m a
getFileName = fromString . srcName <$> getOpt

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

class Monad m => MonadUniq m where
  getUniqSupply :: m UniqSupply
  default getUniqSupply :: (MonadTrans t, MonadUniq m1, m ~ t m1) => m UniqSupply
  getUniqSupply = lift getUniqSupply
  getUniq :: m Int
  default getUniq :: (MonadTrans t, MonadUniq m1, m ~ t m1) => m Int
  getUniq = lift getUniq

instance MonadUniq MalgoM where
  getUniqSupply = get
  getUniq = do
    i <- uniqSupply <$> getUniqSupply
    modify (\s -> s {uniqSupply = i + 1})
    pure i

newtype UniqT m a = UniqT {unUniqT :: StateT UniqSupply m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadFix, MonadFail, MonadIO)

runUniqT :: UniqT m a -> UniqSupply -> m (a, UniqSupply)
runUniqT (UniqT m) = runStateT m

instance Monad m => MonadUniq (UniqT m) where
  getUniqSupply = UniqT get
  getUniq = do
    i <- uniqSupply <$> getUniqSupply
    UniqT $ modify (\s -> s {uniqSupply = i + 1})
    pure i

instance MonadUniq m => MonadUniq (ReaderT r m)

instance MonadUniq m => MonadUniq (ExceptT e m)

instance MonadUniq m => MonadUniq (StateT s m)

instance MonadUniq m => MonadUniq (Lazy.StateT s m)

instance MonadUniq m => MonadUniq (WriterT w m)

instance MonadUniq m => MonadUniq (ModuleBuilderT m)

instance MonadUniq m => MonadUniq (IRBuilderT m)

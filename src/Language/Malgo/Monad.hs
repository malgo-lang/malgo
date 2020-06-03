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
    Colog.Severity (..),
    getFileName,
    viewLine,
    malgoError,
    MonadUniq (..),
  )
where

import Colog
  ( HasLog (..),
    LogAction (..),
    Message,
    Severity (..),
    cfilter,
    richMessageAction,
  )
import qualified Colog
import Control.Monad.Fix
import qualified Data.Text as T
import LLVM.IRBuilder (IRBuilderT, ModuleBuilderT)
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Text.Parsec.Pos (SourcePos, sourceLine)
import Text.PrettyPrint (($$), text)
import qualified Control.Monad.Trans.State.Lazy as Lazy

data Opt
  = Opt
      { srcName :: String,
        dstName :: String,
        dumpParsed :: Bool,
        dumpRenamed :: Bool,
        dumpTyped :: Bool,
        dumpKNormal :: Bool,
        dumpTypeTable :: Bool,
        dumpClosure :: Bool,
        dumpLIR :: Bool,
        dumpDesugar :: Bool,
        dumpLambdaLift :: Bool,
        isInterpretMode :: Bool,
        isDebugMode :: Bool,
        isCoreMode :: Bool
      }
  deriving stock (Eq, Show)

data MalgoEnv m
  = MalgoEnv
      { maOption :: Opt,
        maSource :: Text,
        maLogAction :: LogAction m Message
      }

newtype UniqSupply = UniqSupply {uniqSupply :: Int}

instance HasLog (MalgoEnv m) Message m where
  getLogAction = maLogAction
  setLogAction newLogAction env = env {maLogAction = newLogAction}

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT (MalgoEnv MalgoM) (Lazy.StateT UniqSupply IO) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadReader (MalgoEnv MalgoM), MonadState UniqSupply, MonadIO, MonadFix, MonadFail)

runMalgo :: MonadIO m => MalgoM a -> Opt -> Text -> m a
runMalgo (MalgoM m) opt source =
  liftIO $ Lazy.evalStateT ?? UniqSupply 0 $
    runReaderT
      m
      MalgoEnv
        { maOption = opt,
          maSource = source,
          maLogAction =
            if isDebugMode opt
              then richMessageAction
              else cfilter (\(Colog.Msg sev _ _) -> sev > Colog.Debug) richMessageAction
        }

class Monad m => MonadMalgo m where
  getOpt :: m Opt
  default getOpt :: (MonadTrans t, MonadMalgo m1, m ~ t m1) => m Opt
  getOpt = lift getOpt
  getSource :: m Text
  default getSource :: (MonadTrans t, MonadMalgo m1, m ~ t m1) => m Text
  getSource = lift getSource
  log :: Severity -> Text -> m ()
  default log :: (MonadTrans t, MonadMalgo m1, m ~ t m1) => Severity -> Text -> m ()
  log s t = lift $ log s t

instance MonadMalgo MalgoM where
  getOpt = asks maOption
  getSource = asks maSource
  log = Colog.log

instance MonadMalgo m => MonadMalgo (ReaderT r m)

instance MonadMalgo m => MonadMalgo (ExceptT e m)

instance MonadMalgo m => MonadMalgo (StateT s m)

instance MonadMalgo m => MonadMalgo (WriterT w m)

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

instance MonadUniq m => MonadUniq (ReaderT r m)

instance MonadUniq m => MonadUniq (ExceptT e m)

instance MonadUniq m => MonadUniq (StateT s m)

instance MonadUniq m => MonadUniq (Lazy.StateT s m)

instance MonadUniq m => MonadUniq (WriterT w m)

instance MonadUniq m => MonadUniq (ModuleBuilderT m)

instance MonadUniq m => MonadUniq (IRBuilderT m)

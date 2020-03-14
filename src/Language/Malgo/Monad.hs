{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Monad
  ( MalgoM(..)
  , MalgoEnv(..)
  , runMalgo
  , MonadMalgo(..)
  , Opt(..)
  , Colog.Severity(..)
  , getFileName
  , viewLine
  , malgoError
  , MonadUniq(..)
  )
where

import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Colog                          ( HasLog(..)
                                                , Message
                                                , LogAction(..)
                                                , Severity(..)
                                                , richMessageAction
                                                , cfilter
                                                )
import qualified Colog
import           Control.Monad.Fix
import           Data.List                      ( (!!) )
import           Text.PrettyPrint               ( ($$)
                                                , text
                                                )
import           Text.Parsec.Pos                ( SourcePos
                                                , sourceLine
                                                )

data Opt = Opt
  { srcName       :: String
  , dstName       :: String
  , dumpParsed    :: Bool
  , dumpRenamed   :: Bool
  , dumpTyped     :: Bool
  , dumpKNormal   :: Bool
  , dumpTypeTable :: Bool
  , dumpClosure   :: Bool
  , dumpLIR       :: Bool
  , isDebugMode   :: Bool
  } deriving stock (Eq, Show)

data MalgoEnv m = MalgoEnv
  { maOption :: Opt
  , maSource :: Text
  , maLogAction :: LogAction m Message
  }

newtype UniqSupply = UniqSupply { uniqSupply :: Int }

instance HasLog (MalgoEnv m) Message m where
  getLogAction = maLogAction
  setLogAction newLogAction env = env { maLogAction = newLogAction }

newtype MalgoM a = MalgoM { unMalgoM :: ReaderT (MalgoEnv MalgoM) (StateT UniqSupply IO) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadReader (MalgoEnv MalgoM), MonadState UniqSupply, MonadIO, MonadFix, MonadFail)

runMalgo :: MonadIO m => MalgoM a -> Opt -> Text -> m a
runMalgo (MalgoM m) opt source = liftIO $ evaluatingStateT (UniqSupply 0) $ runReaderT
  m
  MalgoEnv
    { maOption    = opt
    , maSource    = source
    , maLogAction = if isDebugMode opt
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
  getOpt    = asks maOption
  getSource = asks maSource
  log       = Colog.log

instance MonadMalgo m => MonadMalgo (ReaderT r m)
instance MonadMalgo m => MonadMalgo (ExceptT e m)
instance MonadMalgo m => MonadMalgo (StateT s m)
instance MonadMalgo m => MonadMalgo (WriterT w m)

getFileName :: (MonadMalgo m, IsString a) => m a
getFileName = fromString . srcName <$> getOpt

viewLine :: MonadMalgo m => Int -> m Text
viewLine linum = do
  s <- getSource
  pure $ lines s !! (linum - 1)

malgoError :: MonadMalgo m => SourcePos -> Doc -> Doc -> m a
malgoError pos tag mes = do
  line <- viewLine (sourceLine pos)
  errorDoc
    $   "error("
    <>  tag
    <>  "):"
    <+> mes
    $$  "on"
    <+> pPrint pos
    <>  ":"
    $$  text (toString line)
    <>  "\n"

class Monad m => MonadUniq m where
  getUniqSupply :: m UniqSupply
  default getUniqSupply :: (MonadTrans t, MonadUniq m1, m ~ t m1) => m UniqSupply
  getUniqSupply = lift getUniqSupply
  getUniq :: m Int
  default getUniq :: (MonadTrans t, MonadUniq m1, m ~ t m1) => m Int
  getUniq = lift getUniq

instance MonadUniq MalgoM where
  getUniqSupply = get
  getUniq       = do
    i <- uniqSupply <$> getUniqSupply
    modify (\s -> s { uniqSupply = i + 1 })
    pure i

instance MonadUniq m => MonadUniq (ReaderT r m) where
instance MonadUniq m => MonadUniq (ExceptT e m) where
instance MonadUniq m => MonadUniq (StateT s m) where
instance MonadUniq m => MonadUniq (WriterT w m) where

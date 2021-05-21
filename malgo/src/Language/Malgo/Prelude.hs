{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Malgo.Prelude
  ( module Koriel.Prelude,
    runMalgoM,
    Opt (..),
    HasOpt (..),
    MalgoEnv (..),
    HasMalgoEnv (..),
    getOpt,
    errorOn,
    defaultOpt,
    With (..),
    ann,
    value,
  )
where

import Control.Monad.Fix (MonadFix)
import Data.List ((!!))
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import System.FilePath ((-<.>))
import System.IO (readFile)
import Text.Megaparsec.Pos (SourcePos (sourceLine), unPos)

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

data MalgoEnv = MalgoEnv
  { _malgoUniqSupply :: UniqSupply,
    _malgoLogFunc :: LogFunc,
    _malgoOpt :: Opt
  }
  deriving stock (Show, Eq)

class HasMalgoEnv env where
  malgoEnv :: Lens' env MalgoEnv

instance HasMalgoEnv MalgoEnv where
  malgoEnv = lens id const

instance HasUniqSupply MalgoEnv where
  uniqSupply = lens _malgoUniqSupply (\x y -> x {_malgoUniqSupply = y})

instance HasLogFunc MalgoEnv where
  logFuncL = lens _malgoLogFunc (\x y -> x {_malgoLogFunc = y})

instance HasOpt MalgoEnv where
  malgoOpt = lens _malgoOpt (\x y -> x {_malgoOpt = y})

instance Show LogFunc where
  show _ = "LogFunc"

instance Eq LogFunc where
  _ == _ = True

type MalgoM a = RIO MalgoEnv a

deriving newtype instance MonadFix (RIO env)

deriving newtype instance MonadFail (RIO env)

runMalgoM :: MalgoM a -> Opt -> IO a
runMalgoM m opt = do
  uniqSupply <- UniqSupply <$> newIORef 0
  let isVerbose = debugMode opt
  logOptions' <- logOptionsHandle stderr isVerbose
  let logOptions = setLogUseTime True logOptions'

  withLogFunc logOptions \lf -> do
    let app = MalgoEnv {_malgoOpt = opt, _malgoUniqSupply = uniqSupply, _malgoLogFunc = lf}
    runRIO app m

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
      $$ ""

data With x v = With {_ann :: x, _value :: v}
  deriving stock (Eq, Ord, Bounded, Read, Show, Generic)

makeLenses ''With

instance (Pretty x, Pretty v) => Pretty (With x v) where
  pPrintPrec l _ (With x v) = pPrintPrec l 0 v <> brackets (pPrintPrec l 0 x)

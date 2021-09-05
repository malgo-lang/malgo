{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Prelude
  ( module Koriel.Prelude,
    runMalgoM,
    Opt (..),
    HasOpt (..),
    MalgoEnv (..),
    HasMalgoEnv (..),
    getOpt,
    errorOn,
    warningOn,
    defaultOpt,
    With (..),
    ann,
    value,
    ViaAnn (..),
    ViaVal (..),
  )
where

import Control.Monad.Fix (MonadFix)
import Data.List ((!!))
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty
import System.FilePath ((-<.>))
import System.IO (readFile)
import Text.Megaparsec.Pos (SourcePos (..), unPos)

data Opt = Opt
  { srcName :: FilePath,
    dstName :: FilePath,
    dumpParsed :: Bool,
    dumpRenamed :: Bool,
    dumpTyped :: Bool,
    dumpRefine :: Bool,
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
      dumpRefine = False,
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

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader MalgoEnv, MonadFix, MonadFail)

runMalgoM :: MalgoM a -> Opt -> IO a
runMalgoM m opt = do
  uniqSupply <- UniqSupply <$> newIORef 0
  let isVerbose = debugMode opt
  logOptions' <- logOptionsHandle stderr isVerbose
  let logOptions = setLogVerboseFormat True $ setLogUseColor True logOptions'

  withLogFunc logOptions \lf -> do
    let app = MalgoEnv {_malgoOpt = opt, _malgoUniqSupply = uniqSupply, _malgoLogFunc = lf}
    runReaderT (unMalgoM m) app

getOpt :: (HasOpt env, MonadReader env m) => m Opt
getOpt = view malgoOpt

viewLine :: (HasOpt env, MonadReader env m, MonadIO m) => Int -> m String
viewLine linum = do
  srcFileName <- srcName <$> getOpt
  s <- liftIO $ readFile srcFileName
  pure $ lines s !! (linum - 1)

errorOn :: (HasCallStack, HasOpt env, HasLogFunc env, MonadReader env m, MonadIO m) => SourcePos -> Doc -> m a
errorOn pos x = do
  l <- viewLine (unPos $ sourceLine pos)
  let lineNum = unPos $ sourceLine pos
  let columnNum = unPos $ sourceColumn pos
  logError $
    displayShow $
      "error on" <+> pPrint pos <> ":"
        $$ vcat
          [ x,
            nest (length (show lineNum) + 1) "|",
            pPrint lineNum <+> "|" <+> text l,
            nest (length (show lineNum) + 1) "|" <> mconcat (replicate columnNum space) <> "^"
          ]
  exitFailure

warningOn :: (HasLogFunc env, HasOpt env, MonadReader env m, MonadIO m) => SourcePos -> Doc -> m ()
warningOn pos x = do
  l <- viewLine (unPos $ sourceLine pos)
  let lineNum = unPos $ sourceLine pos
  let columnNum = unPos $ sourceColumn pos
  logWarn $
    displayShow $
      "warning on" <+> pPrint pos <> ":"
        $$ vcat
          [ x,
            nest (length (show lineNum) + 1) "|",
            pPrint lineNum <+> "|" <+> text l,
            nest (length (show lineNum) + 1) "|" <> mconcat (replicate columnNum space) <> "^"
          ]

data With x v = With {_ann :: x, _value :: v}
  deriving stock (Eq, Show, Ord)

makeLenses ''With

instance (Pretty x, Pretty v) => Pretty (With v x) where
  pPrintPrec l _ (With v x) = pPrintPrec l 0 v <> brackets (pPrintPrec l 0 x)

newtype ViaAnn value ann = ViaAnn {getViaAnn :: With ann value}

newtype ViaVal ann value = ViaVal {getViaVal :: With ann value}

instance Functor (ViaAnn v) where
  fmap f (ViaAnn (With x v)) = ViaAnn (With (f x) v)

instance Foldable (ViaAnn v) where
  foldMap f (ViaAnn (With x _)) = f x

instance Traversable (ViaAnn v) where
  traverse f (ViaAnn (With x v)) = ViaAnn . (`With` v) <$> f x

instance Functor (ViaVal v) where
  fmap f (ViaVal (With x v)) = ViaVal (With x (f v))

instance Foldable (ViaVal v) where
  foldMap f (ViaVal (With _ v)) = f v

instance Traversable (ViaVal v) where
  traverse f (ViaVal (With x v)) = ViaVal . With x <$> f v

-- [No `instance Bifunctor With'`]
-- Bifunctor have two methods: `first` and `second`.
-- How to map these methods to `ann` and `value`?
-- This problem does not have a good answer.

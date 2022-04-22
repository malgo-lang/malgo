{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Prelude
  ( module Koriel.Prelude,
    runMalgoM,
    Opt (..),
    MalgoEnv (..),
    HasMalgoEnv (..),
    getOpt,
    errorOn,
    warningOn,
    defaultOpt,
    Annotated (..),
    ViaAnn (..),
    ViaVal (..),
  )
where

import Control.Lens (Lens', view)
import Control.Lens.TH
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Data.List ((!!))
import Koriel.Lens
import Koriel.MonadUniq (UniqSupply)
import Koriel.MonadUniq hiding (UniqSupply (_uniqSupply))
import Koriel.Prelude
import Koriel.Pretty
import qualified Koriel.Pretty as P
import System.FilePath ((-<.>))
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

data MalgoEnv = MalgoEnv
  { _uniqSupply :: UniqSupply,
    _opt :: Opt
  }
  deriving stock (Show, Eq)

makeFieldsNoPrefix ''MalgoEnv

class HasMalgoEnv env where
  malgoEnv :: Lens' env MalgoEnv

instance HasMalgoEnv MalgoEnv where
  malgoEnv = identity

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader MalgoEnv, MonadFix, MonadFail, MonadThrow, MonadCatch)

runMalgoM :: MalgoM a -> Opt -> IO a
runMalgoM m opt = do
  uniqSupply <- UniqSupply <$> newIORef 0
  let env = MalgoEnv {_opt = opt, _uniqSupply = uniqSupply}
  runReaderT (unMalgoM m) env

getOpt :: (HasOpt env Opt, MonadReader env m) => m Opt
getOpt = view opt

viewLine :: (HasOpt env Opt, MonadReader env m, MonadIO m) => Int -> m Text
viewLine linum = do
  srcFileName <- srcName <$> getOpt
  s <- readFile srcFileName
  pure $ lines (toText s) !! (linum - 1)

errorOn :: (HasCallStack, HasOpt env Opt, MonadReader env m, MonadIO m) => SourcePos -> Doc -> m a
errorOn pos x = do
  l <- viewLine (unPos $ sourceLine pos)
  let lineNum = unPos $ sourceLine pos
  let columnNum = unPos $ sourceColumn pos
  error $
    show $
      "error on" <+> pPrint pos <> ":"
        $$ vcat
          [ x,
            nest (length (show @String lineNum) + 1) "|",
            pPrint lineNum <+> "|" <+> pPrint l,
            nest (length (show @String lineNum) + 1) "|" <> mconcat (replicate columnNum space) <> "^"
          ]

warningOn :: (HasOpt env Opt, MonadReader env m, MonadIO m) => SourcePos -> Doc -> m ()
warningOn pos x = do
  l <- viewLine (unPos $ sourceLine pos)
  let lineNum = unPos $ sourceLine pos
  let columnNum = unPos $ sourceColumn pos
  hPutStrLn stderr $
    render $
      "warning on" <+> pPrint pos <> ":"
        $$ vcat
          [ x,
            nest (length (show @String lineNum) + 1) "|",
            pPrint lineNum <+> "|" <+> P.text (toString l),
            nest (length (show @String lineNum) + 1) "|" <> mconcat (replicate columnNum space) <> "^"
          ]

data Annotated x v = Annotated {_ann :: x, _value :: v}
  deriving stock (Eq, Show, Ord)

makeFieldsNoPrefix ''Annotated

instance (Pretty x, Pretty v) => Pretty (Annotated v x) where
  pPrintPrec l _ (Annotated v x) = pPrintPrec l 0 v <> brackets (pPrintPrec l 0 x)

newtype ViaAnn value ann = ViaAnn {getViaAnn :: Annotated ann value}

newtype ViaVal ann value = ViaVal {getViaVal :: Annotated ann value}

instance Functor (ViaAnn v) where
  fmap f (ViaAnn (Annotated x v)) = ViaAnn (Annotated (f x) v)

instance Foldable (ViaAnn v) where
  foldMap f (ViaAnn (Annotated x _)) = f x

instance Traversable (ViaAnn v) where
  traverse f (ViaAnn (Annotated x v)) = ViaAnn . (`Annotated` v) <$> f x

instance Functor (ViaVal v) where
  fmap f (ViaVal (Annotated x v)) = ViaVal (Annotated x (f v))

instance Foldable (ViaVal v) where
  foldMap f (ViaVal (Annotated _ v)) = f v

instance Traversable (ViaVal v) where
  traverse f (ViaVal (Annotated x v)) = ViaVal . Annotated x <$> f v

-- [No `instance Bifunctor Annotated'`]
-- Bifunctor have two methods: `first` and `second`.
-- How to map these methods to `ann` and `value`?
-- This problem does not have a good answer.

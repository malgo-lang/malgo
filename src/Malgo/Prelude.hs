{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Prelude
  ( module Koriel.Prelude,
    module Malgo.Prelude,
    -- runMalgoM,
    -- ToLLOpt (..),
    -- MalgoEnv (..),
    -- HasMalgoEnv (..),
    -- Range (..),
    -- errorOn,
    -- warningOn,
    -- defaultToLLOpt,
    -- positionToSourcePos,
    -- sourcePosToPosition,
    -- malgoRangeToLspRange,
    -- malgoRangeToLocation,
  )
where

import Codec.Serialise
import Control.Lens (Lens', view, (^.))
import Control.Lens.TH
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Data.Aeson
import Data.Binary (Binary)
import Data.List ((!!))
import Koriel.Id (ModuleName)
import Koriel.Lens
import Koriel.MonadUniq (UniqSupply)
import Koriel.MonadUniq hiding (UniqSupply (_uniqSupply))
import Koriel.Prelude
import Koriel.Pretty
import Koriel.Pretty qualified as P
import Language.LSP.Types (Position (..), filePathToUri)
import Language.LSP.Types qualified as Lsp
import Language.LSP.Types.Lens (HasEnd (end), HasRange (range), HasStart (start))
import {-# SOURCE #-} Malgo.Interface (Interface)
import System.FilePath ((-<.>))
import Text.Megaparsec.Pos (SourcePos (..), mkPos, unPos)
import Text.Megaparsec.Pos qualified as Megaparsec

data ToLLOpt = ToLLOpt
  { _srcName :: FilePath,
    _dstName :: FilePath,
    _dumpParsed :: Bool,
    _dumpRenamed :: Bool,
    _dumpTyped :: Bool,
    _dumpRefine :: Bool,
    _dumpDesugar :: Bool,
    _noOptimize :: Bool,
    _noLambdaLift :: Bool,
    _inlineSize :: Int,
    _debugMode :: Bool,
    _modulePaths :: [FilePath],
    _forceRebuild :: Bool
  }
  deriving stock (Eq, Show)

makeFieldsNoPrefix ''ToLLOpt

defaultToLLOpt :: FilePath -> ToLLOpt
defaultToLLOpt src =
  ToLLOpt
    { _srcName = src,
      _dstName = src -<.> "ll",
      _dumpParsed = False,
      _dumpRenamed = False,
      _dumpTyped = False,
      _dumpRefine = False,
      _dumpDesugar = False,
      _noOptimize = False,
      _noLambdaLift = False,
      _inlineSize = 10,
      _debugMode = False,
      _modulePaths = [],
      _forceRebuild = False
    }

data MalgoEnv = MalgoEnv
  { _uniqSupply :: UniqSupply,
    _toLLOpt :: ToLLOpt,
    _interfaces :: IORef (HashMap ModuleName Interface)
  }

makeFieldsNoPrefix ''MalgoEnv

class HasMalgoEnv env where
  malgoEnv :: Lens' env MalgoEnv

instance HasMalgoEnv MalgoEnv where
  malgoEnv = identity

instance HasSrcName MalgoEnv FilePath where
  srcName = toLLOpt . srcName

instance HasDstName MalgoEnv FilePath where
  dstName = toLLOpt . dstName

instance HasModulePaths MalgoEnv [FilePath] where
  modulePaths = toLLOpt . modulePaths

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader MalgoEnv, MonadFix, MonadFail, MonadThrow, MonadCatch)

runMalgoM :: MalgoM a -> ToLLOpt -> IO a
runMalgoM m opt = do
  uniqSupply <- UniqSupply <$> newIORef 0
  interfaces <- newIORef mempty
  let env = MalgoEnv {_toLLOpt = opt, _uniqSupply = uniqSupply, _interfaces = interfaces}
  runReaderT (m.unMalgoM) env

viewLine :: (MonadReader env m, MonadIO m, HasSrcName env FilePath) => Int -> m Text
viewLine linum = do
  srcFileName <- view srcName
  s <- readFileBS srcFileName
  pure $ lines (decodeUtf8 s) !! (linum - 1)

-- | Range of a token.
data Range = Range
  { _start :: SourcePos,
    _end :: SourcePos
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Binary Megaparsec.Pos

instance Binary SourcePos

instance Binary Range

instance Hashable Megaparsec.Pos

instance Hashable SourcePos

instance Hashable Range

instance ToJSON Megaparsec.Pos

instance ToJSON SourcePos

instance ToJSON Range

instance FromJSON Megaparsec.Pos

instance FromJSON SourcePos

instance FromJSON Range

instance Serialise Megaparsec.Pos

instance Serialise SourcePos

instance Serialise Range

instance Pretty Range where
  pPrint (Range start end) =
    P.text (sourceName start)
      <> ":"
      <> pPrint (unPos (sourceLine start))
      <> ":"
      <> pPrint (unPos (sourceColumn start))
      <> "-"
      <> pPrint (unPos (sourceLine end))
      <> ":"
      <> pPrint (unPos (sourceColumn end))

makeFieldsNoPrefix ''Range

instance HasRange Range Range where
  range = identity

errorOn :: (HasCallStack, MonadReader env m, MonadIO m, HasSrcName env FilePath) => Range -> Doc -> m a
errorOn range x = do
  l <- viewLine (unPos $ sourceLine $ range ^. start)
  let lineNum = unPos $ sourceLine $ range ^. start
  let columnNum = unPos $ sourceColumn $ range ^. start
  error $
    show $
      "error on"
        <+> pPrint range <> ":"
        $$ vcat
          [ x,
            nest (length (show @String lineNum) + 1) "|",
            pPrint lineNum <+> "|" <+> pPrint l,
            nest (length (show @String lineNum) + 1) "|" <> mconcat (replicate columnNum space) <> "^"
          ]

warningOn :: (MonadReader env m, MonadIO m, HasSrcName env FilePath) => Range -> Doc -> m ()
warningOn range x = do
  l <- viewLine (unPos $ sourceLine $ range ^. start)
  let lineNum = unPos $ sourceLine $ range ^. start
  let columnNum = unPos $ sourceColumn $ range ^. start
  hPutStrLn stderr $
    render $
      "warning on"
        <+> pPrint range <> ":"
        $$ vcat
          [ x,
            nest (length (show @String lineNum) + 1) "|",
            pPrint lineNum <+> "|" <+> P.text (toString l),
            nest (length (show @String lineNum) + 1) "|" <> mconcat (replicate columnNum space) <> "^"
          ]

-- [No `instance Bifunctor Annotated'`]
-- Bifunctor have two methods: `first` and `second`.
-- How to map these methods to `ann` and `value`?
-- This problem does not have a good answer.

positionToSourcePos :: FilePath -> Position -> SourcePos
positionToSourcePos srcName Position {_line, _character} = SourcePos srcName (mkPos $ fromIntegral _line + 1) (mkPos $ fromIntegral _character + 1)

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition SourcePos {sourceLine, sourceColumn} =
  Position (fromIntegral $ unPos sourceLine - 1) (fromIntegral $ unPos sourceColumn - 1)

malgoRangeToLspRange :: Range -> Lsp.Range
malgoRangeToLspRange Range {_start, _end} =
  Lsp.Range (sourcePosToPosition _start) (sourcePosToPosition _end)

malgoRangeToLocation :: Range -> Lsp.Location
malgoRangeToLocation Range {_start, _end} =
  Lsp.Location (filePathToUri (sourceName _start)) $ Lsp.Range (sourcePosToPosition _start) (sourcePosToPosition _end)

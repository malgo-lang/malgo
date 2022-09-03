{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Malgo.Prelude
  ( module Koriel.Prelude,
    module Malgo.Prelude,
  )
where

import Control.Lens.TH
import Control.Monad.Fix (MonadFix)
import Data.Store (Store)
import Error.Diagnose (Marker (This), Position (..), Report (Err, Warn), addFile, addReport, def, defaultStyle, printDiagnostic)
import Koriel.Id (ModuleName)
import Koriel.Lens
import Koriel.MonadUniq (UniqSupply)
import Koriel.Prelude
import Koriel.Pretty
import Koriel.Pretty qualified as P
import Language.LSP.Types (Position (..), filePathToUri)
import Language.LSP.Types qualified as Lsp
import Language.LSP.Types.Lens (HasEnd (end), HasRange (range), HasStart (start))
import {-# SOURCE #-} Malgo.Interface (Interface)
import Text.Megaparsec.Pos (SourcePos (..), mkPos, unPos)
import Text.Megaparsec.Pos qualified as Megaparsec

data MalgoEnv = MalgoEnv
  { _uniqSupply :: UniqSupply,
    _interfaces :: IORef (HashMap ModuleName Interface),
    _srcName :: FilePath,
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
    _modulePaths :: [FilePath]
  }

makeFieldsNoPrefix ''MalgoEnv

newtype MalgoM a = MalgoM {unMalgoM :: ReaderT MalgoEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader MalgoEnv, MonadFix, MonadFail)

runMalgoM :: MalgoEnv -> MalgoM a -> IO a
runMalgoM env m = runReaderT (m.unMalgoM) env

-- | Range of a token.
data Range = Range
  { _start :: SourcePos,
    _end :: SourcePos
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Semigroup Range where
  Range s1 e1 <> Range s2 e2 = Range (min s1 s2) (max e1 e2)

instance Hashable Megaparsec.Pos

instance Hashable SourcePos

instance Hashable Range

instance Store Megaparsec.Pos

instance Store SourcePos

instance Store Range

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

errorOn :: MonadIO m => Range -> Doc -> m a
errorOn range x = do
  let srcFileName = sourceName range._start
  src <- readFileBS srcFileName
  let diag =
        addReport def (Err Nothing "compile error" [(rangeToPosition range, This $ render x)] []) & \diag ->
          addFile diag (sourceName $ range._start) (decodeUtf8 src)
  printDiagnostic stderr True True 4 defaultStyle diag
  exitFailure

rangeToPosition :: Range -> Error.Diagnose.Position
rangeToPosition (Range start end) =
  Error.Diagnose.Position
    { begin = (unPos $ sourceLine start, unPos $ sourceColumn start),
      end = (unPos $ sourceLine end, unPos $ sourceColumn end),
      file = sourceName start
    }

warningOn :: MonadIO m => Range -> Doc -> m ()
warningOn range x = do
  let srcFileName = sourceName range._start
  src <- readFileBS srcFileName
  let diag =
        addReport def (Warn Nothing "compile error" [(rangeToPosition range, This $ render x)] []) & \diag ->
          addFile diag (sourceName $ range._start) (decodeUtf8 src)
  printDiagnostic stderr True True 4 defaultStyle diag
  exitFailure
  where
    rangeToPosition (Range start end) =
      Error.Diagnose.Position
        { begin = (unPos $ sourceLine start, unPos $ sourceColumn start),
          end = (unPos $ sourceLine end, unPos $ sourceColumn end),
          file = sourceName start
        }

-- [No `instance Bifunctor Annotated'`]
-- Bifunctor have two methods: `first` and `second`.
-- How to map these methods to `ann` and `value`?
-- This problem does not have a good answer.

positionToSourcePos :: FilePath -> Lsp.Position -> SourcePos
positionToSourcePos srcName Lsp.Position {_line, _character} = SourcePos srcName (mkPos $ fromIntegral _line + 1) (mkPos $ fromIntegral _character + 1)

sourcePosToPosition :: SourcePos -> Lsp.Position
sourcePosToPosition SourcePos {sourceLine, sourceColumn} =
  Lsp.Position (fromIntegral $ unPos sourceLine - 1) (fromIntegral $ unPos sourceColumn - 1)

malgoRangeToLspRange :: Range -> Lsp.Range
malgoRangeToLspRange Range {_start, _end} =
  Lsp.Range (sourcePosToPosition _start) (sourcePosToPosition _end)

malgoRangeToLocation :: Range -> Lsp.Location
malgoRangeToLocation Range {_start, _end} =
  Lsp.Location (filePathToUri (sourceName _start)) $ Lsp.Range (sourcePosToPosition _start) (sourcePosToPosition _end)

instance HasRange Void Range where
  range _ = absurd
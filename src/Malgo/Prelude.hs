{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Prelude
  ( module Koriel.Prelude,
    module Malgo.Prelude,
  )
where

import Control.Lens.TH
import Data.ByteString qualified as BS
import Data.Store ()
import Data.Store.TH (makeStore)
import Error.Diagnose (Marker (This), Position (..), Report (Err, Warn), TabSize (..), WithUnicode (..), addFile, addReport, defaultStyle, printDiagnostic)
import Koriel.Prelude
import Koriel.Pretty
import Language.LSP.Types (Position (..), filePathToUri)
import Language.LSP.Types qualified as Lsp
import Language.LSP.Types.Lens (HasEnd (end), HasRange (range), HasStart (start))
import System.Exit (exitFailure)
import Text.Megaparsec.Pos (SourcePos (..), mkPos, unPos)
import Text.Megaparsec.Pos qualified as Megaparsec

instance Hashable Megaparsec.Pos

instance Hashable SourcePos

makeStore ''Megaparsec.Pos

makeStore ''SourcePos

-- | Range of a token.
data Range = Range
  { _start :: SourcePos,
    _end :: SourcePos
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

makeStore ''Range

instance Semigroup Range where
  Range s1 e1 <> Range s2 e2 = Range (min s1 s2) (max e1 e2)

instance Pretty Range where
  pretty (Range start end) =
    pretty (convertString @_ @Text $ sourceName start)
      <> ":"
      <> pretty (unPos (sourceLine start))
      <> ":"
      <> pretty (unPos (sourceColumn start))
      <> "-"
      <> pretty (unPos (sourceLine end))
      <> ":"
      <> pretty (unPos (sourceColumn end))

makeFieldsNoPrefix ''Range

instance HasRange Range Range where
  range = identity

errorOn :: (MonadIO m) => Range -> Doc x -> m a
errorOn range x = liftIO do
  let srcFileName = sourceName range._start
  src <- BS.readFile srcFileName
  let diag =
        addReport mempty (Err Nothing "compile error" [(rangeToPosition range, This $ render x)] []) & \diag ->
          addFile diag (sourceName range._start) (convertString src)
  -- TODO: control flags by command line options
  printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle diag
  exitFailure

rangeToPosition :: Range -> Error.Diagnose.Position
rangeToPosition (Range start end) =
  Error.Diagnose.Position
    { begin = (unPos $ sourceLine start, unPos $ sourceColumn start),
      end = (unPos $ sourceLine end, unPos $ sourceColumn end),
      file = sourceName start
    }

warningOn :: (MonadIO m) => Range -> Doc x -> m ()
warningOn range x = liftIO do
  let srcFileName = sourceName range._start
  src <- BS.readFile srcFileName
  let diag =
        addReport mempty (Warn Nothing "compile error" [(rangeToPosition range, This $ render x)] []) & \diag ->
          addFile diag (sourceName range._start) (convertString src)
  printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle diag
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

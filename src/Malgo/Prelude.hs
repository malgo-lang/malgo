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
import Effectful
import Effectful.Reader.Static
import Error.Diagnose (Marker (This), Position (..), Report (Err, Warn), TabSize (..), WithUnicode (..), addFile, addReport, defaultStyle, prettyDiagnostic)
import Koriel.Prelude
import Koriel.Pretty
import Language.LSP.Types (Position (..), filePathToUri)
import Language.LSP.Types qualified as Lsp
import Language.LSP.Types.Lens (HasEnd (end), HasRange (range), HasStart (start))
import Prettyprinter.Render.Text (hPutDoc)
import System.Exit (exitFailure)
import Text.Megaparsec.Pos (SourcePos (..), mkPos, unPos)
import Text.Megaparsec.Pos qualified as Megaparsec

data Flag = Flag
  { noOptimize :: Bool,
    lambdaLift :: Bool,
    debugMode :: Bool,
    testMode :: Bool
  }

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

errorOn :: (Reader Flag :> es, IOE :> es) => Range -> Doc x -> Eff es a
errorOn range x = do
  Flag {testMode} <- ask
  let srcFileName = sourceName range._start
  src <- liftIO $ BS.readFile srcFileName
  let diag =
        addReport mempty (Err Nothing "compile error" [(rangeToPosition range, This $ render x)] []) & \diag ->
          addFile diag (sourceName range._start) (convertString src)
  let doc = (if testMode then unAnnotate else reAnnotate defaultStyle) $ prettyDiagnostic WithUnicode (TabSize 4) diag
  liftIO $ hPutDoc stderr doc
  liftIO exitFailure

rangeToPosition :: Range -> Error.Diagnose.Position
rangeToPosition (Range start end) =
  Error.Diagnose.Position
    { begin = (unPos $ sourceLine start, unPos $ sourceColumn start),
      end = (unPos $ sourceLine end, unPos $ sourceColumn end),
      file = sourceName start
    }

warningOn :: (Reader Flag :> es, IOE :> es) => Range -> Doc x -> Eff es ()
warningOn range x = do
  Flag {testMode} <- ask
  let srcFileName = sourceName range._start
  src <- liftIO $ BS.readFile srcFileName
  let diag =
        addReport mempty (Warn Nothing "compile error" [(rangeToPosition range, This $ render x)] []) & \diag ->
          addFile diag (sourceName range._start) (convertString src)
  let doc = (if testMode then unAnnotate else reAnnotate defaultStyle) $ prettyDiagnostic WithUnicode (TabSize 4) diag
  liftIO $ hPutDoc stderr doc
  liftIO exitFailure
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

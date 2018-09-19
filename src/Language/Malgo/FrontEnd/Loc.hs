{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.Loc
  ( SrcSpan(..)
  , SrcInfo(..)
  , Loc(..)
  , Line
  , Column
  ) where

import           Data.Outputable
import           Language.Malgo.Pretty
import           Universum

type Line = Int
type Column = Int

data SrcSpan = SrcSpan
  { srcSpanFilename    :: FilePath
  , srcSpanStartLine   :: Line
  , srcSpanStartColumn :: Column
  , srcSpanEndLine     :: Line
  , srcSpanEndColumn   :: Column
  } deriving (Eq, Ord, Show)

instance Pretty SrcSpan where
  pPrint s = sep $ punctuate colon
    [pPrint (filename s)
    , "line:" <+> pPrint (startLine s) <> "-" <> pPrint (endLine s)
    , "column:" <+> pPrint (startColumn s) <> "-" <> pPrint (endColumn s)]

instance Outputable SrcSpan where
  pprPrec _ = parens . pPrint

class SrcInfo a where
  srcSpan :: a -> SrcSpan
  filename :: a -> FilePath
  filename = srcSpanFilename . srcSpan
  startLine :: a -> Line
  startLine = srcSpanStartLine . srcSpan
  startColumn :: a -> Column
  startColumn = srcSpanStartColumn . srcSpan
  endLine :: a -> Line
  endLine = srcSpanEndLine . srcSpan
  endColumn :: a -> Column
  endColumn = srcSpanEndColumn . srcSpan

instance (SrcInfo a, SrcInfo b) => SrcInfo (a, b) where
  srcSpan (x, y) = SrcSpan
    { srcSpanFilename = filename x
    , srcSpanStartLine = startLine x
    , srcSpanStartColumn = startColumn x
    , srcSpanEndLine = endLine y
    , srcSpanEndColumn = endColumn y
    }

instance SrcInfo SrcSpan where
  srcSpan = id

data Loc a = Loc
  { loc   :: SrcSpan
  , unLoc :: a
  } deriving (Ord, Show)

instance Eq a => Eq (Loc a) where
  x == y = unLoc x == unLoc y

instance Pretty a => Pretty (Loc a) where
  pPrint = pPrint . unLoc

instance SrcInfo (Loc a) where
  srcSpan = loc

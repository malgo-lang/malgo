{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.Loc
  ( SrcSpan(..)
  , SrcInfo(..)
  , Loc(..)
  , Line
  , Column
  , srcSpan
  ) where

import           Language.Malgo.Pretty

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

class SrcInfo a where
  filename :: a -> FilePath
  startLine :: a -> Line
  startColumn :: a -> Column
  endLine :: a -> Line
  endColumn :: a -> Column

srcSpan :: (SrcInfo a, SrcInfo b) => a -> b -> SrcSpan
srcSpan s e = SrcSpan
  { srcSpanFilename = filename s
  , srcSpanStartLine = startLine s
  , srcSpanStartColumn = startColumn s
  , srcSpanEndLine = endLine s
  , srcSpanEndColumn = endColumn e
  }

instance SrcInfo SrcSpan where
  filename = srcSpanFilename
  startLine = srcSpanStartLine
  startColumn = srcSpanStartColumn
  endLine = srcSpanEndLine
  endColumn = srcSpanEndColumn

data Loc a = Loc
  { loc   :: SrcSpan
  , unLoc :: a
  } deriving (Ord, Show)

instance Eq a => Eq (Loc a) where
  x == y = unLoc x == unLoc y

instance Pretty a => Pretty (Loc a) where
  pPrint = pPrint . unLoc

instance SrcInfo (Loc a) where
 filename = filename . loc
 startLine = startLine . loc
 startColumn = startColumn . loc
 endLine = endLine . loc
 endColumn = endColumn . loc

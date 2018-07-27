{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.Loc
  ( SrcSpan(..)
  , SrcInfo(..)
  , Loc(..)
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
  pPrint srcSpan = sep $ punctuate colon
    [pPrint (filename srcSpan)
    , "line:" <+> pPrint (startLine srcSpan) <> "-" <> pPrint (endLine srcSpan)
    , "column:" <+> pPrint (startColumn srcSpan) <> "-" <> pPrint (endColumn srcSpan)]

class SrcInfo a where
  filename :: a -> FilePath
  startLine :: a -> Line
  startColumn :: a -> Column
  endLine :: a -> Line
  endColumn :: a -> Column

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

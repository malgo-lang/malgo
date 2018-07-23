{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.FrontEnd.Info where

import           Data.Text.Prettyprint.Doc hiding (line, column)
import           RIO

newtype Info = Info (Text, Int, Int)
  deriving (Show, Eq, Ord, Read)

instance Pretty Info where
  pretty (Info (file, line, column)) =
    parens $ sep $ punctuate "," [pretty file, "line" <+> pretty line, "column" <+> pretty column]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData                 #-}
module Language.Malgo.FrontEnd.Info where

import           RIO
import qualified RIO.Text as Text
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

newtype Info = Info (Text, Int, Int)
  deriving (Show, Eq, Ord, Read)

instance Pretty Info where
  pPrint (Info (file, line, column)) =
    parens $ sep $ punctuate "," [text (Text.unpack file), "line" <+> int line, "column" <+> int column]

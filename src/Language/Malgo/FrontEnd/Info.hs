{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.FrontEnd.Info where

import           Language.Malgo.Pretty
import           RIO

newtype Info = Info (Text, Int, Int)
  deriving (Show, Eq, Ord, Read)

instance Pretty Info where
  pPrint (Info (file, line, column)) =
    parens $ sep $ punctuate "," [pPrint file, "line" <+> pPrint line, "column" <+> pPrint column]

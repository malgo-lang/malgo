{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Malgo.FrontEnd.Info where

import           Language.Malgo.Pretty
import           Relude

newtype Info = Info (Text, Int, Int)
  deriving (Show, Eq, Ord, Read, Generic, PrettyVal)

instance Pretty Info where
  pPrint (Info (file, line, column)) =
    parens $ sep $ punctuate "," [pPrint file, "line" <+> pPrint line, "column" <+> pPrint column]

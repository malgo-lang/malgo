{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
module Language.Malgo.FrontEnd.Info where

import           Data.Outputable
import           Language.Malgo.Pretty
import           Relude

newtype Info = Info (Text, Int, Int)
  deriving (Show, Eq, Ord, Read, Generic, Outputable)

instance Pretty Info where
  pPrint (Info (file, line, column)) =
    parens $ sep $ punctuate "," [pPrint file, "line" <+> pPrint line, "column" <+> pPrint column]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.FrontEnd.Info where

import           Data.Outputable
import           Language.Malgo.Pretty
import           RIO

newtype Info = Info (Text, Int, Int)
  deriving (Show, Eq, Ord, Read, Generic, Outputable)

instance Pretty Info where
  pPrint (Info (file, line, column)) =
    parens $ sep $ punctuate "," [pPrint file, "line" <+> pPrint line, "column" <+> pPrint column]

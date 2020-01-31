{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Malgo.FrontEnd.Info where

import           Language.Malgo.Pretty
import           Language.Malgo.Prelude
import           Text.PrettyPrint.HughesPJClass ( parens
                                                , sep
                                                , punctuate
                                                )

newtype Info = Info (Text, Int, Int)
  deriving stock (Show, Eq, Ord, Read, Generic)

instance Pretty Info where
  pPrint (Info (file, line, column)) =
    parens $ sep $ punctuate "," [pPrint file, "line" <+> pPrint line, "column" <+> pPrint column]

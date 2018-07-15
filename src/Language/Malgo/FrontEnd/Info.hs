{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
module Language.Malgo.FrontEnd.Info where

import           Data.Text.Prettyprint.Doc
import           RIO

newtype Info = Info (Text, Int, Int)
  deriving (Show, Read, Pretty)

instance Eq Info where
  _ == _ = True

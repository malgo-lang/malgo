{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Utils where

import Language.Malgo.Prelude
import qualified Text.PrettyPrint           as P

type Name = Text

fromName :: StringConv Name a => Name -> a
fromName = toS

newtype Info = Info (Text, Int, Int)
  deriving (Show, Read)

instance Eq Info where
  _ == _ = True

instance PrettyPrint Info where
  pretty (Info x) = P.text $ show x

instance HasDummy Info where
  dummy = Info ("<dummy>", 0, 0)

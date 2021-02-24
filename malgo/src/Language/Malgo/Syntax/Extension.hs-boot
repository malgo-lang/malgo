{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Language.Malgo.Syntax.Extension where

import Data.Binary (Binary)
import Koriel.Pretty (Pretty)
import Language.Malgo.Prelude

newtype ModuleName = ModuleName String

instance Eq ModuleName

instance Ord ModuleName

instance Show ModuleName

instance Generic ModuleName

instance Binary ModuleName

instance Pretty ModuleName

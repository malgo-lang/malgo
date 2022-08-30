{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Koriel.Pretty
  ( module Text.PrettyPrint.HughesPJClass,
    (<+>),
    errorDoc,
  )
where

import Data.Fix
import Data.String.Conversions (convertString)
import Koriel.Prelude
import Text.Megaparsec.Pos qualified as Megaparsec
import Text.PrettyPrint.HughesPJClass hiding ({- char, -} double, first, float, int, integer, (<+>), (<>))
import Text.PrettyPrint.HughesPJClass qualified as P
import Prelude qualified

-- change operator precedence
infixl 9 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (P.<+>)

instance Pretty Doc where
  pPrint = identity

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ P.render x

instance Pretty (f (Fix f)) => Pretty (Fix f) where
  pPrintPrec l d (Fix f) = pPrintPrec l d f

-- Pretty SourcePos
instance Pretty Megaparsec.SourcePos where
  pPrint = text . Megaparsec.sourcePosPretty

instance Pretty Text where
  pPrint = text . convertString

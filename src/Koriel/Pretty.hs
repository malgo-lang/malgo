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
import qualified Text.Megaparsec.Pos as Megaparsec
import Text.PrettyPrint.HughesPJClass hiding (char, double, first, float, int, integer, (<+>), (<>))
import qualified Text.PrettyPrint.HughesPJClass as P
import qualified Prelude

-- change operator precedence
infixl 9 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (P.<+>)

instance Pretty Doc where
  pPrint = id

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ P.render x

instance Pretty (f (Fix f)) => Pretty (Fix f) where
  pPrintPrec l d (Fix f) = pPrintPrec l d f

-- Pretty SourcePos
instance Pretty Megaparsec.SourcePos where
  pPrint = text . Megaparsec.sourcePosPretty

instance Pretty Text where
  pPrint = text . convertString
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Koriel.Pretty
  ( module Text.PrettyPrint.HughesPJClass,
    Pretty1 (..),
    (<+>),
    errorDoc,
    pretty,
    rendered,
    toText,
  )
where

import Data.Fix
import Koriel.Prelude
import Text.PrettyPrint.HughesPJClass hiding (char, double, first, float, int, integer, (<+>), (<>))
import qualified Text.PrettyPrint.HughesPJClass as P
import qualified Prelude

-- change operator precedence
infixl 9 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (P.<+>)

instance Pretty Doc where
  pPrint = id

pretty :: Pretty a => Getter a Doc
pretty = to pPrint

rendered :: Getter Doc String
rendered = to P.render

toText :: Pretty a => Getter a String
toText = pretty . rendered

errorDoc ::
#ifdef DEBUG
  HasCallStack => Doc -> a
errorDoc x = Prelude.error $ P.render x
#else
  Doc -> a
errorDoc x = Prelude.errorWithoutStackTrace $ P.render x
#endif

class Pretty1 f where
  liftPPrintPrec :: (PrettyLevel -> Rational -> a -> Doc) -> PrettyLevel -> Rational -> f a -> Doc

instance Pretty1 f => Pretty (Fix f) where
  pPrintPrec l d (Fix f) = liftPPrintPrec pPrintPrec l d f

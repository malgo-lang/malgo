{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Koriel.Pretty
  ( module Text.PrettyPrint.HughesPJClass,
    (<+>),
    pShow,
    errorDoc,
    pretty,
    rendered,
    toText,
  )
where

import Koriel.Prelude
import Text.Pretty.Simple (pShow)
import Text.PrettyPrint.HughesPJClass hiding (char, double, first, float, int, integer, (<+>), (<>)) -- (Doc, Pretty (..))
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

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ P.render x

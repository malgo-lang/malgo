{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Malgo.Pretty
  ( module Text.PrettyPrint.HughesPJClass,
    (<+>),
    pShow,
    errorDoc,
    errorOn,
    pretty,
    rendered,
    toText,
  )
where

import Koriel.Prelude
import Text.Pretty.Simple (pShow)
import Text.PrettyPrint.HughesPJClass (Doc, Pretty (..))
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
toText = pretty % rendered

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ P.render x

errorOn :: (HasCallStack, Pretty a) => a -> Doc -> b
errorOn pos x = errorDoc $ "error on" <+> pPrint pos <> ":" P.$+$ P.nest 2 x
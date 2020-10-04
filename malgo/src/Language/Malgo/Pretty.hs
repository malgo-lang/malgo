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

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Optics
import Koriel.Prelude
import Text.Pretty.Simple (pShow)
import Text.PrettyPrint.HughesPJClass (Doc, Pretty (..))
import qualified Text.PrettyPrint.HughesPJClass as P
import qualified Prelude

-- change operator precedence
infixl 9 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = (P.<+>)

instance Pretty T.Text where
  pPrint = P.text . T.unpack

instance Pretty TL.Text where
  pPrint = P.text . TL.unpack

instance Pretty Doc where
  pPrint = id

pretty :: Pretty a => Getter a Doc
pretty = to pPrint

rendered :: IsText a => Getter Doc a
rendered = to P.render % packed

toText :: (Pretty t, IsText a) => Getter t a
toText = pretty % rendered

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ P.render x

errorOn :: (HasCallStack, Pretty a) => a -> Doc -> b
errorOn pos x = errorDoc $ "error on" <+> pPrint pos <> ":" P.$+$ P.nest 2 x
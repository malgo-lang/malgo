{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Malgo.Pretty
  ( module Text.PrettyPrint.HughesPJClass,
    (<+>),
    pShow,
    errorDoc,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Language.Malgo.Prelude
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

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ P.render x

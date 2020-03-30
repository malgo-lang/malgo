{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Pretty
  ( module Text.PrettyPrint.HughesPJClass
  , (<+>)
  , pShow
  , errorDoc
  )
where

import           Language.Malgo.Prelude

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Prelude
import           Text.Pretty.Simple             ( pShow )
import           Text.PrettyPrint.HughesPJClass ( Pretty(..)
                                                , Doc
                                                )
import qualified Text.PrettyPrint.HughesPJClass
                                               as P

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

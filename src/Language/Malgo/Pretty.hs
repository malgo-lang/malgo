{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Pretty
  ( module Text.PrettyPrint.HughesPJClass
  , (<+>)
  , pShow
  , errorDoc
  )
where

import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import qualified Prelude
import           Relude
import           Text.Pretty.Simple             (pShow)
import           Text.PrettyPrint.HughesPJClass hiding (double, empty, (<+>),
                                                 (<>))
import qualified Text.PrettyPrint.HughesPJClass as P

-- change operator precedence
infixl 9 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) = (P.<+>)

instance Pretty T.Text where
  pPrint = text . T.unpack

instance Pretty TL.Text where
  pPrint = text . TL.unpack

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ render x

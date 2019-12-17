{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Pretty
  ( module Text.PrettyPrint.HughesPJClass
  , (<+>)
  , module Text.Show.Pretty
  , errorDoc
  )
where

import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import           Text.PrettyPrint.HughesPJClass hiding (double, empty, (<+>),
                                                 (<>))
import qualified Text.PrettyPrint.HughesPJClass as P
import           Text.Show.Pretty               (PrettyVal (..), dumpDoc,
                                                 dumpIO, dumpStr)
import qualified Text.Show.Pretty               as S
import Relude
import qualified Prelude

-- change operator precedence
infixl 9 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) = (P.<+>)

instance Pretty T.Text where
  pPrint = text . T.unpack

instance Pretty TL.Text where
  pPrint = text . TL.unpack

instance PrettyVal () where
  prettyVal _ = S.Tuple []

errorDoc :: HasCallStack => Doc -> a
errorDoc x = Prelude.error $ render x

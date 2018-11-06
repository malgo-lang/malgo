{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Malgo.Pretty
  ( module Text.PrettyPrint.HughesPJClass
  , (<+>)
  )
where

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.PrettyPrint.HughesPJClass
                                         hiding ( (<+>)
                                                , (<>)
                                                )
import qualified Text.PrettyPrint.HughesPJClass
                                               as P
import           Universum

-- change operator precedence
infixl 9 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) = (P.<+>)

instance Pretty T.Text where
  pPrint = text . toString

instance Pretty TL.Text where
  pPrint = text . toString

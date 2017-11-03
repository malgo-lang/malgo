module Language.Malgo.PrettyPrint where

import           Text.PrettyPrint

class PrettyPrint a where
  pretty :: a -> Doc

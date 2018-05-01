module Language.Malgo.ID
  ( ID(..) ) where

import           Language.Malgo.Prelude
import qualified Text.PrettyPrint       as P

data ID = ID { _name :: Name, _uniq :: Int }
  deriving (Show, Ord, Read)

instance Eq ID where
  x == y = _uniq x == _uniq y

instance PrettyPrint ID where
  pretty (ID name u) = pretty name <> P.text "." <> P.int u

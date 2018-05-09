module Language.Malgo.ID
  ( ID(..) ) where

import           Language.Malgo.Prelude
import qualified Text.PrettyPrint       as P

data ID = ID { _name :: Name, _uniq :: Int }
  deriving (Show, Ord, Read)

instance Eq ID where
  x == y = _uniq x == _uniq y

instance Outputable ID where
  ppr (ID name u) = ppr name <> P.text "." <> P.int u

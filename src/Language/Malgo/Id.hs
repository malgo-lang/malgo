module Language.Malgo.Id where

import           Language.Malgo.Monad
import           Universum

data Id = Id { _name :: Text, _uniq :: Int }
    deriving (Show, Eq, Ord)

newId :: MonadMalgo f => Text -> f Id
newId name = Id name <$> newUniq

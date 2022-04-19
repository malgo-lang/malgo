module Koriel.Lens where

import Control.Lens

class HasUniqSupply s a | s -> a where
  uniqSupply :: Lens' s a

class HasOpt s a | s -> a where
  opt :: Lens' s a

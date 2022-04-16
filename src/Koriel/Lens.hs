module Koriel.Lens where

import Control.Lens

class HasBody s a | s -> a where
  body :: Lens' s a

class HasPatterns s a | s -> a where
  patterns :: Lens' s a

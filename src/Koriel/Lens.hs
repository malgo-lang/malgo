module Koriel.Lens where

import Control.Lens

class HasUniqSupply s a | s -> a where
  uniqSupply :: Lens' s a

class HasOpt s a | s -> a where
  opt :: Lens' s a

class HasMeta s a | s -> a where
  meta :: Lens' s a

class HasName s a | s -> a where
  name :: Lens' s a

class HasSort s a | s -> a where
  sort :: Lens' s a

class HasUniq s a | s -> a where
  uniq :: Lens' s a

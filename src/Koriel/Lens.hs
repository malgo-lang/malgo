module Koriel.Lens where

import Control.Lens

class HasBody s a | s -> a where
  body :: Lens' s a

class HasPatterns s a | s -> a where
  patterns :: Lens' s a

class HasParamTypes s a | s -> a where
  paramTypes :: Lens' s a

class HasReturnType s a | s -> a where
  returnType :: Lens' s a

class HasConstructorInfoMap s a | s -> a where
  constructorInfoMap :: Lens' s a

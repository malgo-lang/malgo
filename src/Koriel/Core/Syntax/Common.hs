module Koriel.Core.Syntax.Common (HasFreeVar (..)) where

import Koriel.Prelude

-- | 'f' may have free variables
class HasFreeVar f where
  -- | free variables
  freevars :: Hashable a => f a -> HashSet a

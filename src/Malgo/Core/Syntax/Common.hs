module Malgo.Core.Syntax.Common (HasFreeVar (..)) where

import Malgo.Prelude

-- | 'f' may have free variables
-- 'freevars' does not include callees of `call-direct`.
-- If you want to include callees of `call-direct`, merge 'callees' and 'freevars'.
class HasFreeVar f where
  -- | Free variables.
  -- It does not include callees of `call-direct`.
  freevars :: (Hashable a) => f a -> HashSet a

  -- | Callees.
  callees :: (Hashable a) => f a -> HashSet a

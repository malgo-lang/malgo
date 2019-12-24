{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.MiddleEnd.FreeVars
  ( FreeVars(..) )
where

import           Language.Malgo.Prelude

class FreeVars f where
  freevars :: Ord a => f a -> Set a

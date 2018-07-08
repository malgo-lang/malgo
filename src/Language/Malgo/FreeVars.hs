{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.FreeVars (FreeVars(..))where

import           Data.List              (nub)
import RIO

class FreeVars f where
  freevars :: (Show a, Ord a) => f a -> [a]

  fv :: (Show a, Ord a) => f a -> [a]
  fv x = nub (freevars x)

{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.MiddleEnd.FreeVars where

import           Relude

class FreeVars f where
  freevarsPrec :: Ord a => f a -> [a]

  freevars :: Ord a => f a -> [a]
  freevars x = ordNub (freevarsPrec x)

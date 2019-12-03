{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.MiddleEnd.FreeVars (FreeVars(..), delete, (\\)) where

import           Data.Set (delete, (\\))
import           Relude

class FreeVars f where
  freevars :: Ord a => f a -> Set a

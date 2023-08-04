module Malgo.Rename.RnState (RnState (..)) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Syntax.Extension

data RnState = RnState
  { infixInfo :: HashMap RnId (Assoc, Int),
    dependencies :: HashSet ModuleName
  }
  deriving stock (Show)

instance Pretty RnState where
  pretty RnState {infixInfo, dependencies} =
    "RnState"
      <+> braces
        ( sep
            [ sep ["infixInfo", "=", pretty $ HashMap.toList infixInfo],
              sep ["dependencies", "=", pretty $ HashSet.toList dependencies]
            ]
        )
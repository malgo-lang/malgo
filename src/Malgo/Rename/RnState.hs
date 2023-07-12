module Malgo.Rename.RnState (RnState (..), infixInfo, dependencies) where

import Control.Lens (Lens', lens)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Syntax.Extension

data RnState = RnState
  { _infixInfo :: HashMap RnId (Assoc, Int),
    _dependencies :: HashSet ModuleName
  }
  deriving stock (Show)

instance Pretty RnState where
  pretty RnState {_infixInfo, _dependencies} =
    "RnState"
      <+> braces
        ( sep
            [ sep ["_infixInfo", "=", pretty $ HashMap.toList _infixInfo],
              sep ["_dependencies", "=", pretty $ HashSet.toList _dependencies]
            ]
        )

infixInfo :: Lens' RnState (HashMap RnId (Assoc, Int))
infixInfo = lens (._infixInfo) (\r x -> r {_infixInfo = x})

dependencies :: Lens' RnState (HashSet ModuleName)
dependencies = lens (._dependencies) (\r x -> r {_dependencies = x})

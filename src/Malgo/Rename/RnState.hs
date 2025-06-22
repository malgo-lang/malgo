module Malgo.Rename.RnState (RnState (..)) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Malgo.Module
import Malgo.Prelude
import Malgo.Syntax.Extension
import Prettyprinter (braces, sep, (<+>))

data RnState = RnState
  { infixInfo :: Map RnId (Assoc, Int),
    dependencies :: Set ModuleName,
    exportedIdentifiers :: [PsId],
    exportedTypeIdentifiers :: [PsId]
  }
  deriving stock (Show)

instance Pretty RnState where
  pretty RnState {infixInfo, dependencies, exportedIdentifiers, exportedTypeIdentifiers} =
    "RnState"
      <+> braces
        ( sep
            [ sep ["infixInfo", "=", pretty $ Map.toList infixInfo],
              sep ["dependencies", "=", pretty $ Set.toList dependencies],
              sep ["exportedIdentifiers", "=", pretty exportedIdentifiers],
              sep ["exportedTypeIdentifiers", "=", pretty exportedTypeIdentifiers]
            ]
        )

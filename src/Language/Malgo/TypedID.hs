module Language.Malgo.TypedID
  ( TypedID
  ) where

import           Language.Malgo.ID
import           Language.Malgo.Type

type TypedID = ID Type

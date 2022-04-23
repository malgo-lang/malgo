{-# LANGUAGE TemplateHaskell #-}

module Malgo.Lsp.Index where

import Control.Lens.TH
import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import qualified Data.HashMap.Strict as HashMap
import Koriel.Lens
import Malgo.Prelude
import Malgo.TypeRep (Scheme, Type)
import Text.Megaparsec.Pos (SourcePos)

-- | A 'Index' is a mapping from 'Info' to '[Range]'.
newtype Index = Index {unwrapIndex :: HashMap Info [Range]}
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance Binary Index

-- | An 'Info' records
--  * Symbol name
--  * Type
--  * Definition
data Info = Info
  { _name :: Text,
    _typeSignature :: Scheme Type,
    _definitions :: [Range]
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Binary Info

instance Hashable Info

makeFieldsNoPrefix ''Info

-- | 'findInfosOfPos' finds all 'Info's that are corresponding to the given 'SourcePos'.
findInfosOfPos :: SourcePos -> Index -> [Info]
findInfosOfPos pos (Index index) =
  HashMap.keys $
    HashMap.filter (any (isInRange pos)) index

isInRange :: SourcePos -> Range -> Bool
isInRange pos Range {_start, _end} = pos >= _start && pos <= _end

{-# LANGUAGE TemplateHaskell #-}

module Malgo.Lsp.Index where

import Control.Lens.TH
import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import qualified Data.HashMap.Strict as HashMap
import Koriel.Lens
import Koriel.Pretty
import Malgo.Prelude
import Malgo.TypeRep (Scheme, Type)
import Text.Megaparsec.Pos (SourcePos (..), Pos)

-- | A 'Index' is a mapping from 'Info' to '[Range]' (references).
newtype Index = Index {unwrapIndex :: HashMap Info [Range]}
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance Binary Index

instance Pretty Index where
  pPrint = pPrint . HashMap.toList . unwrapIndex

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

instance Pretty Info where
  pPrint Info {..} = pPrint _name <+> ":" <+> pPrint _typeSignature <+> pPrint _definitions

makeFieldsNoPrefix ''Info

-- | 'findInfosOfPos' finds all 'Info's that are corresponding to the given 'SourcePos'.
-- It ignores file names.
findInfosOfPos :: SourcePos -> Index -> [Info]
findInfosOfPos pos (Index index) =
  HashMap.keys $
    HashMap.filter (any (isInRange pos)) index

isInRange :: SourcePos -> Range -> Bool
isInRange pos Range {_start, _end} = posToTuple _start <= posToTuple pos && posToTuple pos < posToTuple _end
  where
    posToTuple :: SourcePos -> (Pos, Pos)
    posToTuple SourcePos {sourceLine, sourceColumn} = (sourceLine, sourceColumn)
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Lsp.Index where

import Control.Lens.TH
import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import qualified Data.HashMap.Strict as HashMap
import Koriel.Lens
import Koriel.Pretty
import Malgo.Prelude
import Malgo.Syntax.Extension (RnId)
import Malgo.TypeRep (Scheme, Type)
import System.FilePath (takeFileName)
import Text.Megaparsec.Pos (Pos, SourcePos (..))

-- | A 'Index' is a mapping from 'Info' to '[Range]' (references).
data Index = Index
  { _references :: HashMap Info [Range],
    _definitionMap :: HashMap RnId Info
  }
  deriving stock (Show, Generic)

instance Semigroup Index where
  (Index refs1 defs1) <> (Index refs2 defs2) = Index (refs1 <> refs2) (defs1 <> defs2)

instance Monoid Index where
  mempty = Index mempty mempty

instance Binary Index

instance Pretty Index where
  pPrint = pPrint . HashMap.toList . _references

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
makeFieldsNoPrefix ''Index

-- | 'findInfosOfPos' finds all 'Info's that are corresponding to the given 'SourcePos'.
-- It ignores file names.
findInfosOfPos :: SourcePos -> Index -> [Info]
findInfosOfPos pos (Index refs _) =
  HashMap.keys $
    HashMap.filter (any (isInRange pos)) refs

isInRange :: SourcePos -> Range -> Bool
isInRange pos Range {_start, _end}
  | takeFileName (sourceName pos) == takeFileName (sourceName _start) = posToTuple _start <= posToTuple pos && posToTuple pos < posToTuple _end
  | otherwise = False
  where
    posToTuple :: SourcePos -> (Pos, Pos)
    posToTuple SourcePos {sourceLine, sourceColumn} = (sourceLine, sourceColumn)
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Malgo.Lsp.Index where

import Control.Lens.TH
import Data.Aeson
import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import Data.HashMap.Strict qualified as HashMap
import Koriel.Pretty
import Language.LSP.Types (DocumentSymbol (..))
import Malgo.Infer.TypeRep (Scheme, Type)
import Malgo.Prelude
import Malgo.Syntax.Extension (RnId)
import System.FilePath (takeFileName)
import Text.Megaparsec.Pos (Pos, SourcePos (..))
import Text.Pretty.Simple (pShow)

data Index = Index
  { _references :: HashMap Info [Range],
    _definitionMap :: HashMap RnId Info,
    _symbolInfo :: HashMap RnId DocumentSymbol
  }
  deriving stock (Show, Generic)

instance Semigroup Index where
  (Index refs1 defs1 syms1) <> (Index refs2 defs2 syms2) = Index (refs1 <> refs2) (defs1 <> defs2) (syms1 <> syms2)

instance Monoid Index where
  mempty = Index mempty mempty mempty

instance ToJSON Index

instance FromJSON Index

instance Pretty Index where
  pPrint = text . toString . pShow

-- | An 'Info' records
--  * Symbol name
--  * Type
--  * Definition
data Info = Info
  { _name :: Text,
    typeSignature :: Scheme Type,
    definitions :: [Range]
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Binary Info

instance ToJSON Info

instance ToJSONKey Info

instance FromJSON Info

instance FromJSONKey Info

instance Hashable Info

instance Pretty Info where
  pPrint Info {..} = pPrint _name <+> ":" <+> pPrint typeSignature <+> pPrint definitions

makeFieldsNoPrefix ''Info
makeFieldsNoPrefix ''Index

-- | 'findInfosOfPos' finds all 'Info's that are corresponding to the given 'SourcePos'.
-- It ignores file names.
findInfosOfPos :: SourcePos -> Index -> [Info]
findInfosOfPos pos (Index refs _ _) =
  HashMap.keys $
    HashMap.filter (any (isInRange pos)) refs

isInRange :: SourcePos -> Range -> Bool
isInRange pos Range {_start, _end}
  | takeFileName (sourceName pos) == takeFileName (sourceName _start) = posToTuple _start <= posToTuple pos && posToTuple pos < posToTuple _end
  | otherwise = False
  where
    posToTuple :: SourcePos -> (Pos, Pos)
    posToTuple SourcePos {sourceLine, sourceColumn} = (sourceLine, sourceColumn)
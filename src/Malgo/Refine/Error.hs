module Malgo.Refine.Error (RefineError(..)) where

import Malgo.Infer.TypeRep (Type, TypeVar)
import Malgo.Prelude
import Malgo.Refine.Space (Space)
import Prettyprinter (nest, squotes, brackets, vsep, (<+>))

-- | Errors that can occur during the refine pass
data RefineError
  = InvalidInstantiation Range TypeVar Type
  | TypeMismatch Range Type Type
  | NonExhaustivePatterns Range [Space]
  deriving stock (Eq, Show)

instance Pretty RefineError where
  pretty (InvalidInstantiation range v typ) =
    pretty range <> ": Invalid instantiation:" <+> squotes (pretty v)
      <+> "can't be instantiated with" <+> pretty typ
  pretty (TypeMismatch range t1 t2) =
    pretty range <> ": Type mismatch:" <+> pretty t1 <+> "and" <+> pretty t2
      <+> "are not the same"
  pretty (NonExhaustivePatterns range spaces) =
    vsep
      [ pretty range <> ": Pattern is not exhaustive:",
        nest 2 (pretty spaces)
      ]

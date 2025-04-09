{-# OPTIONS_GHC -Wno-partial-fields #-}

module Malgo.Infer.Error (InferError (..)) where

import Malgo.Id
import Malgo.Infer.TypeRep
import Malgo.Prelude
import Prettyprinter (sep, squotes, (<+>))

data InferError
  = NotInScope {range :: Range, name :: Id}
  | SignatureTooGeneral {range :: Range, declaredScheme :: Scheme Type, inferredScheme :: Scheme Type}
  | SignatureMismatch {range :: Range, expectedScheme :: Scheme Type, actualScheme :: Scheme Type}
  | PatternSizeMismatch {range :: Range, expected :: Int, actual :: Int}
  | MissingParentheses {range :: Range}
  | OccursCheckFailed {range :: Range, var :: MetaVar, typ :: Type}
  | UnificationError {range :: Range, expectedType :: Type, actualType :: Type}
  | IterationLimitExceeded {range :: Range}
  | InvalidTypeApplication {range :: Range, callee :: Type, arg :: Type}

instance Pretty InferError where
  pretty NotInScope {..} =
    sep [pretty range <> ":", "Not in scope:" <+> squotes (pretty name)]
  pretty SignatureTooGeneral {..} =
    sep
      [ pretty range <> ":",
        "Signature too general:",
        "expected" <+> pretty declaredScheme,
        "but got" <+> pretty inferredScheme
      ]
  pretty SignatureMismatch {..} =
    sep
      [ pretty range <> ":",
        "Signature mismatch:",
        "expected" <+> pretty expectedScheme,
        "but got" <+> pretty actualScheme
      ]
  pretty PatternSizeMismatch {..} =
    sep
      [ pretty range <> ":",
        "Pattern size mismatch:",
        "expected" <+> pretty expected,
        "but got" <+> pretty actual
      ]
  pretty MissingParentheses {..} =
    sep
      [ pretty range <> ":",
        "Missing parentheses"
      ]
  pretty OccursCheckFailed {..} =
    sep
      [ pretty range <> ":",
        "Occurs check failed:"
          <+> pretty var <> "occurs in"
          <+> pretty typ
      ]
  pretty (UnificationError {..}) =
    sep
      [ pretty range <> ":",
        "Unification error:"
          <+> "cannot unify"
          <+> pretty expectedType
          <+> "with"
          <+> pretty actualType
      ]
  pretty IterationLimitExceeded {..} =
    sep
      [ pretty range <> ":",
        "Iteration limit exceeded"
      ]
  pretty InvalidTypeApplication {..} =
    sep
      [ pretty range <> ":",
        "Invalid type application:"
          <+> pretty callee
          <+> "cannot be applied to"
          <+> pretty arg
      ]

instance Show InferError where
  show = show . pretty
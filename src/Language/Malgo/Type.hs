module Language.Malgo.Type where

import           Language.Malgo.Pretty
import           Prelude               (show)
import           Universum             hiding (Type)

data TypeScheme a = Forall [a] (Type a)
  deriving (Eq, Show)

instance Pretty a => Pretty (TypeScheme a) where
  pPrint (Forall xs ty) = "forall" <+> sep (map pPrint xs) <> "." <+> pPrint ty

newtype TyRef a = TyRef (IORef (Maybe (Type a)))
  deriving Eq

instance Show (TyRef a) where
  show _ = "<TyRef>"

instance Pretty (TyRef a) where
  pPrint _ = "<TyRef>"

data Type a = TyApp TyCon [Type a]
            | TyVar a
            | TyMeta (TyRef a)
  deriving (Eq, Show)

instance Pretty a => Pretty (Type a) where
  pPrintPrec l d (TyApp ArrowC [x, y]) =
    maybeParens (d > 5) $ pPrintPrec l 6 x <+> "->" <+> pPrintPrec l 6 y
  pPrintPrec _ _ (TyApp (TupleC n) xs)
    | length xs == n = parens $ sep $ punctuate "," $ map pPrint xs
  pPrintPrec l d (TyApp con args) =
    maybeParens (d > 10) $ pPrint con <+> sep (map (pPrintPrec l 11) args)
  pPrintPrec _ _ (TyVar x) = pPrint x
  pPrintPrec _ _ (TyMeta x) = pPrint x

data PrimC = IntC | DoubleC | CharC | BoolC | StringC
  deriving (Eq, Show)

instance Pretty PrimC where
  pPrint IntC    = "Int"
  pPrint DoubleC = "Double"
  pPrint CharC   = "Char"
  pPrint BoolC   = "Bool"
  pPrint StringC = "String"

data TyCon = TupleC Int
           | ArrowC
           | ArrayC
           | PrimC PrimC
  deriving (Eq, Show)

instance Pretty TyCon where
  pPrint (TupleC n) = "(" <> text (replicate n ',') <> ")"
  pPrint ArrowC     = "(->)"
  pPrint ArrayC     = "Array"
  pPrint (PrimC c)  = pPrint c

module Language.Malgo.PrettyPrint.HIR where

import           Language.Malgo.HIR
import           Language.Malgo.PrettyPrint.Syntax (prettyOp, prettyType)
import           Language.Malgo.Syntax             (Type)
import           Text.PrettyPrint

prettyId :: Id -> Doc
prettyId (Sym s) = text s

pretty :: HIR a -> Doc
pretty (HIR d) = prettyDECL d

prettyDECL :: DECL a -> Doc
prettyDECL (DEF i t v) =
  parens $ text "def" <+> parens (prettyId i <+> prettyType t) <+> prettyEXPR v
prettyDECL (DEFUN i retTy params body) =
  parens $ text "def" <+> parens (prettyId i <+> prettyType retTy)
  <+> parens (prettyParam params)
  $+$ nest 4 (prettyEXPR body)
prettyDECL (EXDEF i t) = parens $ text "extern" <+> parens (prettyId i <+> prettyType t)
prettyDECL (EXDEFUN i retTy params) =
  parens $ text "def" <+> parens (prettyId i <+> prettyType retTy)
  <+> parens (prettyParam params)

prettyParam :: [(Id, Type)] -> Doc
prettyParam []          = empty
prettyParam ((i, t):xs) = parens (prettyId i <+> prettyType t) <+> prettyParam xs

prettyEXPR :: EXPR a -> Doc
prettyEXPR (e, _) = prettyEXPR' e

prettyEXPR' :: EXPR' a -> Doc
prettyEXPR' (VAR x)       = prettyId x
prettyEXPR' (INT x)       = int x
prettyEXPR' (FLOAT x)     = double x
prettyEXPR' (BOOL True)   = text "#t"
prettyEXPR' (BOOL False)  = text "#f"
prettyEXPR' (CHAR x)      = char x
prettyEXPR' (STRING x)    = doubleQuotes $ text x
prettyEXPR' UNIT          = text "unit"
prettyEXPR' (CALL i args) = parens $ prettyId i <+> hsep (map prettyEXPR args)
prettyEXPR' (LET (Sym "#_") _ e1 e2) = prettyEXPR e1 $$ prettyEXPR e2
prettyEXPR' (LET i t v b) =
  parens $ text "let"
  <+> parens (prettyId i <+> prettyType t <+> prettyEXPR v)
  $$ nest (-1) (prettyEXPR b)
prettyEXPR' (IF c t f) =
  parens $ text "if" <+> prettyEXPR c
  $+$ nest 2 (braces (prettyEXPR t))
  $+$ nest 2 (braces (prettyEXPR f))

prettyEXPR' (BINOP o x y) =
  parens $ prettyOp o <+> prettyEXPR x <+> prettyEXPR y

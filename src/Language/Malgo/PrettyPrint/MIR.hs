module Language.Malgo.PrettyPrint.MIR where

import           Language.Malgo.HIR                (Id)
import           Language.Malgo.MIR
import           Language.Malgo.PrettyPrint.HIR    (prettyId)
import           Language.Malgo.PrettyPrint.Syntax (prettyOp, prettyType)
import           Language.Malgo.Syntax             (Type)
import           Text.PrettyPrint

pretty :: DECL -> Doc
pretty (DEF name typ val) =
  text "def" <+> prettyId name <> colon <> prettyType typ <+> prettyEXPR val
pretty (DEFUN fn retTy params body) =
  text "def" <+> prettyId fn <+> parens (prettyParams params)
  <> colon <> prettyType retTy
  $+$ text "entry:" $$ nest 2 (prettyEXPR body)
pretty (EXDEF name typ) =
  text "extern" <+> prettyId name <> colon <> prettyType typ
pretty (EXDEFUN fn retTy params) =
  text "extern" <+> prettyId fn <+> prettyType retTy <+> parens (prettyParams params)

prettyParams :: [(Id, Type)] -> Doc
prettyParams []          = empty
prettyParams ((x, t):xs) = prettyId x <> colon <> prettyType t <+> prettyParams xs

prettyEXPR :: EXPR -> Doc
prettyEXPR (VAR i)        = prettyId i
prettyEXPR (INT x)        = int x
prettyEXPR (FLOAT x)      = double x
prettyEXPR (BOOL True)    = text "#t"
prettyEXPR (BOOL False)   = text "#f"
prettyEXPR (CHAR x)       = quotes (char x)
prettyEXPR (STRING x)     = doubleQuotes (text x)
prettyEXPR UNIT           = text "unit"
prettyEXPR (CALL fn args) = prettyId fn <> parens (cat (map prettyId args))
prettyEXPR (LET name typ val body) =
  prettyId name <> colon <> prettyType typ <+> text "="
  <+> prettyEXPR val $+$ prettyEXPR body
prettyEXPR (BINOP op x y) = text "binop" <+> prettyOp op <+> prettyId x <+> prettyId y
prettyEXPR (IF c t f) = text "if" <+> prettyId c
  $+$ text "then:" $$ nest 2 (prettyEXPR t)
  $+$ text "else:" $$ nest 2 (prettyEXPR f)

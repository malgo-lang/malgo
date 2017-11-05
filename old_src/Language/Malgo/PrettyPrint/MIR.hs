module Language.Malgo.PrettyPrint.MIR where

import           Language.Malgo.HIR                (Id (..))
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
  $+$ prettyBLOCK body
pretty (EXDEF name typ) =
  text "extern" <+> prettyId name <> colon <> prettyType typ
pretty (EXDEFUN fn retTy params) =
  text "extern" <+> prettyId fn <+> prettyType retTy <+> parens (prettyParams params)

prettyParams :: [(Id, Type)] -> Doc
prettyParams []          = empty
prettyParams ((x, t):xs) = prettyId x <> colon <> prettyType t <+> prettyParams xs

prettyBLOCK :: BLOCK -> Doc
prettyBLOCK (BLOCK (Sym label) es) =
  text label <> colon
  $+$ nest 2 (vcat (map prettyEXPR es))

prettyEXPR :: EXPR -> Doc
prettyEXPR (VAR i, _)        = prettyId i
prettyEXPR (INT x, _)        = int x
prettyEXPR (FLOAT x, _)      = double x
prettyEXPR (BOOL True, _)    = text "#t"
prettyEXPR (BOOL False, _)   = text "#f"
prettyEXPR (CHAR x, _)       = quotes (char x)
prettyEXPR (STRING x, _)     = doubleQuotes (text x)
prettyEXPR (UNIT, _)           = text "unit"
prettyEXPR (CALL fn args, _) = text "call" <+> prettyType (snd fn) <+> prettyId (fst fn) <> parens (cat (map (prettyId . fst) args))
prettyEXPR (LET name typ val, _) =
  prettyId name <> colon <> prettyType typ <+> text "="
  <+> prettyEXPR val
prettyEXPR (BINOP op x y, _) = text "binop" <+> prettyOp op <+> prettyId x <+> prettyId y
prettyEXPR (IF ret c t f, _) = text "if" <> colon <> prettyId ret <+> prettyId c
  $+$ prettyBLOCK t
  $+$ prettyBLOCK f
prettyEXPR (RET name ty, _) = text "ret" <+> prettyType ty <+> prettyId name
prettyEXPR (IFRET name ty val, _) = text "ifret"
  <+> prettyId name <> colon <> prettyType ty
  <+> text "=" <+> prettyEXPR val
prettyEXPR (NOP, _) = empty

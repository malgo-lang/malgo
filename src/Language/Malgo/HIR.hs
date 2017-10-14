{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Language.Malgo.HIR where

import           Language.Malgo.Syntax (Name, Op, Type (..))
-- import           Text.PrettyPrint

newtype Id = Sym String
  deriving (Eq, Show)

newtype HIR (a :: Phase) = HIR { unHIR :: DECL a }
  deriving (Eq, Show)

data Phase = Typed | KNormal
  deriving (Eq, Show)

data DECL (a :: Phase) = DEF Id Type (EXPR a)
                       | DEFUN Id Type [(Id, Type)] (EXPR a)
                       | EXDEF Id Type
                       | EXDEFUN Id Type [(Id, Type)]
  deriving (Eq, Show)

data EXPR' (a :: Phase) = VAR Id
                        | INT Int
                        | FLOAT Double
                        | BOOL Bool
                        | CHAR Char
                        | STRING String
                        | UNIT
                        | CALL { _callName :: Id
                               , _callArgs :: [EXPR a]
                               }
                        | LET { _letName  :: Id
                              , _letType  :: Type
                              , _letValue :: EXPR a
                              , _letBody  :: EXPR a}
                        | IF (EXPR a) (EXPR a) (EXPR a)
                        | BINOP Op (EXPR a) (EXPR a)
  deriving (Eq, Show)

type EXPR (a :: Phase) = (EXPR' a, Type)

name2Id :: Name -> Id
name2Id = Sym

-- TODO: もっと見やすいプリティプリンタ
-- prettyHIR :: HIR a -> Doc
-- prettyHIR (HIR decl) = prettyDECL decl

-- prettyDECL :: DECL a -> Doc
-- prettyDECL (DEF i t e) = text "def" <+> prettyId i <> colon <> prettyType t <+> equals <+> prettyEXPR e
-- prettyDECL (DEFUN i t params body) =
--   text "def"
--   <+> prettyId i
--   <> prettyParams params <> colon <> prettyType t
--   <+> equals <+> lbrace
--   $+$ nest 4 (prettyEXPR body)
--   $+$ rbrace
-- prettyDECL (EXDEF i t) = text "extern" <+> prettyId i <> colon <> prettyType t
-- prettyDECL (EXDEFUN i t params) = text "extern" <+> prettyId i <> prettyParams params <> colon <> prettyType t

-- prettyParams :: [(Id, Type)] -> Doc
-- prettyParams params =
--   parens (sep (punctuate comma
--                (map
--                  (\(i, ty) -> prettyId i <> colon <> prettyType ty)
--                  params)))

-- prettyId :: Id -> Doc
-- prettyId (Sym n) = text n

-- prettyEXPR :: EXPR a -> Doc
-- prettyEXPR (e, _) = prettyEXPR' e

-- prettyEXPR' :: EXPR' a -> Doc
-- prettyEXPR' (VAR x)    = prettyId x
-- prettyEXPR' (INT x)    = int x
-- prettyEXPR' (FLOAT x)  = double x
-- prettyEXPR' (BOOL True) = text "#t"
-- prettyEXPR' (BOOL False) = text "#f"
-- prettyEXPR' (CHAR x)   = quotes $ char x
-- prettyEXPR' (STRING x) = doubleQuotes $ text x
-- prettyEXPR' UNIT = text "unit"
-- prettyEXPR' (CALL i args) =
--   prettyId i <> parens (sep $ punctuate comma (map prettyEXPR args))
-- prettyEXPR' (SEQ x (UNIT, UnitTy)) = prettyEXPR x <> semi
-- prettyEXPR' (SEQ x@(SEQ{}, _) y) = braces (prettyEXPR x) <> semi
--                               $+$ prettyEXPR y
-- prettyEXPR' (SEQ x y) = braces $ prettyEXPR x <> semi $+$ prettyEXPR y
-- prettyEXPR' (LET i t val) =
--   text "let" <+> prettyId i <> colon <> prettyType t
--   <+> equals <+> prettyEXPR val
-- prettyEXPR' (IF c t f) = text "if"
--   <+> prettyEXPR c
--   <+> lbrace
--   $+$ nest 4 (prettyEXPR t)
--   $+$ rbrace
--   <+> text "else"
--   <+> lbrace
--   $+$ nest 4 (prettyEXPR f)
--   $+$ rbrace
-- prettyEXPR' (BINOP op e1 e2) = prettyEXPR e1 <+> prettyOp op <+> prettyEXPR e2

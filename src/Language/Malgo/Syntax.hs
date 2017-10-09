{-# LANGUAGE StrictData #-}

module Language.Malgo.Syntax where

import           Control.Lens     ()
import           Text.Parsec.Pos
import           Text.PrettyPrint (($+$), (<+>), (<>))
import qualified Text.PrettyPrint as P

type Name = String

mkName :: String -> Name
mkName = id

type Pos = SourcePos

data Decl = Def Pos Name Type Expr
          | Defun Pos Name Type [(Name, Type)] Expr
          | ExDef Pos Name Type
          | ExDefun Pos Name Type [(Name, Type)]
  deriving (Eq, Show)

data Expr = Var Pos Name
          | Int Pos Int
          | Float Pos Double
          | Bool Pos Bool
          | Char Pos Char
          | String Pos String
          | Unit Pos
          | Call Pos Name [Expr]
          | Seq Pos Expr Expr
          | Let Pos Name Type Expr
          | If Pos Expr Expr Expr
          | BinOp Pos Op Expr Expr
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge | And | Or
  deriving (Eq, Show)

data Type = IntTy
          | FloatTy
          | BoolTy
          | CharTy
          | StringTy
          | UnitTy
          | FunTy Type [Type]
  deriving (Eq, Show)

prettyExpr :: Expr -> P.Doc
prettyExpr (Int _ n)    = P.int n
prettyExpr (Float _ x)  = P.double x
prettyExpr (Char _ x)   = P.quotes $ P.char x
prettyExpr (String _ x) = P.doubleQuotes $ P.text x
prettyExpr (Unit _)       = P.text "unit"
prettyExpr (Call _ name args) = P.text name <> P.parens (P.sep $ P.punctuate P.comma (map prettyExpr args))
prettyExpr (Seq _ x (Unit _)) = prettyExpr x <> P.semi
prettyExpr (Seq _ x y) = prettyExpr x <> P.semi $+$ prettyExpr y
prettyExpr (Let _ name ty val) =
  P.text "let" <+> P.text name <> P.colon <> prettyType ty
  <+> P.equals <+> prettyExpr val
prettyExpr (Var _ name) = P.text name
prettyExpr (Bool _ True) = P.text "#t"
prettyExpr (Bool _ False) = P.text "#f"
prettyExpr (If _ c t f) = P.text "if"
  <+> prettyExpr c
  <+> P.lbrace
  $+$ P.nest 4 (prettyExpr t)
  $+$ P.rbrace
  <+> P.text "else"
  <+> P.lbrace
  $+$ P.nest 4 (prettyExpr f)
  $+$ P.rbrace
prettyExpr (BinOp _ Add x y) = prettyExpr' x <+> P.char '+' <+> prettyExpr' y
prettyExpr (BinOp _ Sub x y) = prettyExpr' x <+> P.char '-' <+> prettyExpr' y
prettyExpr (BinOp _ Mul x y) = prettyExpr' x <+> P.char '*' <+> prettyExpr' y
prettyExpr (BinOp _ Div x y) = prettyExpr' x <+> P.char '/' <+> prettyExpr' y
prettyExpr (BinOp _ Eq x y) = prettyExpr' x <+> P.text "==" <+> prettyExpr' y
prettyExpr (BinOp _ Neq x y) = prettyExpr' x <+> P.text "/=" <+> prettyExpr' y
prettyExpr (BinOp _ Lt x y) = prettyExpr' x <+> P.char '<' <+> prettyExpr' y
prettyExpr (BinOp _ Gt x y) = prettyExpr' x <+> P.char '>' <+> prettyExpr' y
prettyExpr (BinOp _ Le x y) = prettyExpr' x <+> P.text "<=" <+> prettyExpr' y
prettyExpr (BinOp _ Ge x y) = prettyExpr' x <+> P.text ">=" <+> prettyExpr' y
prettyExpr (BinOp _ And x y) = prettyExpr' x <+> P.text "&&" <+> prettyExpr' y
prettyExpr (BinOp _ Or x y) = prettyExpr' x <+> P.text "||" <+> prettyExpr' y

prettyExpr' :: Expr -> P.Doc
prettyExpr' (BinOp _ Add x y) = P.parens $ prettyExpr' x <+> P.char '+' <+> prettyExpr' y
prettyExpr' (BinOp _ Sub x y) = P.parens $ prettyExpr' x <+> P.char '-' <+> prettyExpr' y
prettyExpr' (BinOp _ Mul x y) = P.parens $ prettyExpr' x <+> P.char '*' <+> prettyExpr' y
prettyExpr' (BinOp _ Div x y) = P.parens $ prettyExpr' x <+> P.char '/' <+> prettyExpr' y
prettyExpr' (BinOp _ Eq x y) = P.parens $ prettyExpr' x <+> P.text "==" <+> prettyExpr' y
prettyExpr' (BinOp _ Neq x y) = P.parens $ prettyExpr' x <+> P.text "/=" <+> prettyExpr' y
prettyExpr' (BinOp _ Lt x y) = P.parens $ prettyExpr' x <+> P.char '<' <+> prettyExpr' y
prettyExpr' (BinOp _ Gt x y) = P.parens $ prettyExpr' x <+> P.char '>' <+> prettyExpr' y
prettyExpr' (BinOp _ Le x y) = P.parens $ prettyExpr' x <+> P.text "<=" <+> prettyExpr' y
prettyExpr' (BinOp _ Ge x y) = P.parens $ prettyExpr' x <+> P.text ">=" <+> prettyExpr' y
prettyExpr' (BinOp _ And x y) = P.parens $ prettyExpr' x <+> P.text "&&" <+> prettyExpr' y
prettyExpr' (BinOp _ Or x y) = P.parens $ prettyExpr' x <+> P.text "||" <+> prettyExpr' y
prettyExpr' x = prettyExpr x

prettyType :: Type -> P.Doc
prettyType IntTy              = P.text "Int"
prettyType FloatTy            = P.text "Float"
prettyType BoolTy             = P.text "Bool"
prettyType CharTy             = P.text "Char"
prettyType StringTy           = P.text "String"
prettyType UnitTy             = P.text "Unit"
prettyType (FunTy ret params) =
  P.sep (P.punctuate (P.text " *") (map prettyType params))
  <+> P.text "->"
  <+> prettyType ret

prettyDecl :: Decl -> P.Doc
prettyDecl (Def _ name ty val) = P.text "def"
  <+> P.text name <> P.colon <> prettyType ty <+> P.equals <+> prettyExpr val
prettyDecl (Defun _ name ret params body) = P.text "def"
  <+> P.text name
  <> P.parens (P.sep (P.punctuate P.comma (map prettyParam params)))
  <> P.colon <> prettyType ret
  <+> P.equals <+> P.lbrace
  $+$ P.nest 4 (prettyExpr body)
  $+$ P.rbrace
prettyDecl (ExDef _ name ty) = P.text "extern" <+> P.text name <> P.colon <> prettyType ty
prettyDecl (ExDefun _ name ret params) = P.text "extern" <+> P.text name
  <> P.parens (P.sep (P.punctuate P.comma (map prettyParam params)))
  <> P.colon <> prettyType ret

prettyParam :: (Name, Type) -> P.Doc
prettyParam (n, ty) = P.text n <> P.colon <> prettyType ty

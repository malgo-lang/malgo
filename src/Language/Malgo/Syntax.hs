{-# LANGUAGE StrictData #-}
module Language.Malgo.Syntax where

import           Text.PrettyPrint (($+$), (<+>), (<>))
import qualified Text.PrettyPrint as P

type Name = String

mkName :: String -> Name
mkName = id

data Decl = Def Name Type Expr
          | Defun Name Type [(Name, Type)] Expr
  deriving (Eq, Show)

data Expr = Var Name
          | Int Int
          | Float Double
          | Bool Bool
          | Char Char
          | String String
          | Unit
          | Call Name [Expr]
          | Seq Expr Expr
          | Let Name Type Expr
          | If Expr Expr Expr
          | BinOp Op Expr Expr
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Eq | Lt | Gt | Le | Ge | And | Or
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
prettyExpr (Int n)    = P.int n
prettyExpr (Float x)  = P.double x
prettyExpr (Char x)   = P.quotes $ P.char x
prettyExpr (String x) = P.doubleQuotes $ P.text x
prettyExpr Unit       = P.text "unit"
prettyExpr (Call name args) = P.text name <> P.parens (P.sep $ P.punctuate P.comma (map prettyExpr args))
prettyExpr (Seq x Unit) = prettyExpr x <> P.semi
prettyExpr (Seq x y) = prettyExpr x <> P.semi $+$ prettyExpr y
prettyExpr (Let name ty val) = P.text "let" <+>
                               P.text name <>
                               P.colon <>
                               prettyType ty <+>
                               P.equals <+>
                               prettyExpr val
prettyExpr (Var name) = P.text name
prettyExpr (Bool True) = P.text "#t"
prettyExpr (Bool False) = P.text "#f"
prettyExpr (If c t f) = P.text "if"
  <+> prettyExpr c
  <+> P.lbrace
  $+$ P.nest 4 (prettyExpr t)
  $+$ P.rbrace
  <+> P.text "else"
  <+> P.lbrace
  $+$ P.nest 4 (prettyExpr f)
  $+$ P.rbrace
prettyExpr (BinOp Add x y) = prettyExpr x <+> P.char '+' <+> prettyExpr y
prettyExpr (BinOp Sub x y) = prettyExpr x <+> P.char '-' <+> prettyExpr y
prettyExpr (BinOp Mul x y) = prettyExpr x <+> P.char '*' <+> prettyExpr y
prettyExpr (BinOp Div x y) = prettyExpr x <+> P.char '/' <+> prettyExpr y
prettyExpr (BinOp Eq x y) = prettyExpr x <+> P.text "==" <+> prettyExpr y
prettyExpr (BinOp Lt x y) = prettyExpr x <+> P.char '<' <+> prettyExpr y
prettyExpr (BinOp Gt x y) = prettyExpr x <+> P.char '>' <+> prettyExpr y
prettyExpr (BinOp Le x y) = prettyExpr x <+> P.text "<=" <+> prettyExpr y
prettyExpr (BinOp Ge x y) = prettyExpr x <+> P.text ">=" <+> prettyExpr y
prettyExpr (BinOp And x y) = prettyExpr x <+> P.text "&&" <+> prettyExpr y
prettyExpr (BinOp Or x y) = prettyExpr x <+> P.text "||" <+> prettyExpr y

prettyType :: Type -> P.Doc
prettyType IntTy              = P.text "Int"
prettyType FloatTy            = P.text "Float"
prettyType BoolTy             = P.text "Bool"
prettyType CharTy             = P.text "Char"
prettyType StringTy           = P.text "String"
prettyType UnitTy             = P.text "Unit"
prettyType (FunTy ret params) =
  P.text "Fun"
  <+> P.brackets (P.sep $
                   P.punctuate P.comma (map prettyType params))
  <+> prettyType ret

prettyDecl :: Decl -> P.Doc
prettyDecl (Def name ty val) = P.text "def"
  <+> P.text name <> P.colon <> prettyType ty <+> P.equals <+> prettyExpr val
prettyDecl (Defun name ret params body) = P.text "def"
  <+> P.text name
  <> P.parens (P.sep (P.punctuate P.comma (map prettyParam params)))
  <> P.colon <> prettyType ret
  <+> P.equals <+> P.lbrace
  $+$ P.nest 4 (prettyExpr body)
  $+$ P.rbrace
  where prettyParam (n, ty) = P.text n <> P.colon <> prettyType ty

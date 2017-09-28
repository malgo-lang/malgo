module Language.Malgo.Syntax where

import           Text.PrettyPrint (($$), ($+$), (<+>), (<>))
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
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Eq Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr
          | Le Expr Expr
          | Ge Expr Expr
          | And Expr Expr
          | Or Expr Expr
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
prettyExpr (If c t f) = P.text "if" <+>
                        prettyExpr c <+>
                        P.lbrace $+$
                        P.nest 4 (prettyExpr t) $+$
                        P.rbrace <+>
                        P.text "else" <+>
                        P.lbrace $+$
                        P.nest 4 (prettyExpr f) $+$
                        P.rbrace
prettyExpr (Add x y) = prettyExpr x <+> P.char '+' <+> prettyExpr y
prettyExpr (Sub x y) = prettyExpr x <+> P.char '-' <+> prettyExpr y
prettyExpr (Mul x y) = prettyExpr x <+> P.char '*' <+> prettyExpr y
prettyExpr (Div x y) = prettyExpr x <+> P.char '/' <+> prettyExpr y
prettyExpr (Eq x y) = prettyExpr x <+> P.text "==" <+> prettyExpr y
prettyExpr (Lt x y) = prettyExpr x <+> P.char '<' <+> prettyExpr y
prettyExpr (Gt x y) = prettyExpr x <+> P.char '>' <+> prettyExpr y
prettyExpr _          = undefined

prettyType :: Type -> P.Doc
prettyType IntTy = P.text "Int"

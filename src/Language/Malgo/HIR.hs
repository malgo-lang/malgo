{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.HIR where

-- import           Data.String
import           Language.Malgo.Syntax (Type (..))
import           Language.Malgo.Utils
import           Text.PrettyPrint

{-
HIR is simlar to Language.Malgo.Syntax.Expr.
But HIR does'nt has "Info" property and `ExDec`.
HIR.Let has only one Decl.
-}

data Expr a =
  Var a
  | Int Integer
  | Float Double
  | Bool Bool
  | Char Char
  | String String
  | Unit
  | Call a [a]
  | Let (Decl a) (Expr a)
  | If a (Expr a) (Expr a)
  | BinOp Op a a
  deriving (Eq, Show)

instance PrettyPrint a => PrettyPrint (Expr a) where
  pretty (Var x)         = pretty x
  pretty (Int x)         = integer x
  pretty (Float x)       = double x
  pretty (Bool True)     = text "#t"
  pretty (Bool False)    = text "#f"
  pretty (Char x)        = quotes $ char x
  pretty (String x)      = doubleQuotes $ text x
  pretty Unit            = text "()"
  pretty (Call fn arg)   = parens $ pretty fn <+> sep (map pretty arg)
  pretty (Let decl body) =
    parens $ text "let" <+> parens (pretty decl)
    $+$ nest 2 (pretty body)
  pretty (If c t f) =
    parens $ text "if" <+> pretty c
    $+$ nest 2 (pretty t)
    $+$ nest 2 (pretty f)
  pretty (BinOp op x y) = parens $ sep [pretty op, pretty x, pretty y]

-- | 中置演算子の種類を表すタグ
data Op = Add
        | Sub
        | Mul
        | Div
        | FAdd
        | FSub
        | FMul
        | FDiv
        | Mod
        | Eq Type
        | Neq Type
        | Lt Type
        | Gt Type
        | Le Type
        | Ge Type
        | And
        | Or
  deriving (Eq, Show)

instance PrettyPrint Op where
  pretty Add      = text "+"
  pretty Sub      = text "-"
  pretty Mul      = text "*"
  pretty Div      = text "/"
  pretty FAdd     = text "+."
  pretty FSub     = text "-."
  pretty FMul     = text "*."
  pretty FDiv     = text "/."
  pretty Mod      = text "%"
  pretty (Eq ty)  = text "==" <> brackets (pretty ty)
  pretty (Neq ty) = text "<>" <> brackets (pretty ty)
  pretty (Lt ty)  = text "<" <> brackets (pretty ty)
  pretty (Gt ty)  = text ">" <> brackets (pretty ty)
  pretty (Le ty)  = text "<=" <> brackets (pretty ty)
  pretty (Ge ty)  = text ">=" <> brackets (pretty ty)
  pretty And      = text "&&"
  pretty Or       = text "||"

data Decl a = FunDec a [a] (Expr a)
            | ValDec a (Expr a)
  deriving (Eq, Show)

instance PrettyPrint a => PrettyPrint (Decl a) where
  pretty (FunDec name params body) =
    text "fun" <+> (parens . sep $ pretty name
                    : map pretty params)
    $+$ nest 2 (pretty body)
  pretty (ValDec name val) =
    text "val" <+> pretty name
    <+> pretty val

data Extern a = ExDec a String
  deriving (Eq, Show)

instance PrettyPrint a => PrettyPrint (Extern a) where
  pretty (ExDec name orig) = parens $
    text "extern" <+> pretty name
    <+> text orig

data Program a = Program { _externs  :: [Extern a]
                         , _toplevel :: [Decl a]
                         , _body     :: Expr a
                         }

instance PrettyPrint a => PrettyPrint (Program a) where
  pretty (Program e t b) = sep (map pretty e) $+$ sep (map pretty t) $+$ pretty b

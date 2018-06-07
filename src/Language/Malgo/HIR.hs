{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Language.Malgo.HIR where

import           Language.Malgo.Prelude
import           Language.Malgo.Type

{-
HIR is simlar to Language.Malgo.Syntax.Expr.
But HIR does'nt has "Info" property and `ExDec`.
HIR.Let has only one Decl.
-}
data Expr a = Var a
            | Int Integer
            | Float Double
            | Bool Bool
            | Char Char
            | String Text
            | Unit
            | Tuple [a]
            | TupleAccess a Int
            | Call a [a]
            | Let (Decl a) (Expr a)
            | If a (Expr a) (Expr a)
            | BinOp Op a a
    deriving (Eq, Show, Read)

instance Pretty a => Pretty (Expr a) where
  pretty (Var x) = pretty x
  pretty (Int x) = pretty x
  pretty (Float x) = pretty x
  pretty (Bool True) = "#t"
  pretty (Bool False) = "#f"
  pretty (Char x) = squotes $ pretty x
  pretty (String x) = dquotes $ pretty x
  pretty (Tuple xs) =
    braces $ sep $ punctuate "," $ map pretty xs
  pretty (TupleAccess xs i) =
    parens ("." <+> pretty xs <+> pretty i)
  pretty Unit = "{}"
  pretty (Call fn arg) =
    parens $ pretty fn <+> sep (map pretty arg)
  pretty (Let decl body) =
    parens $ "let" <+> parens (pretty decl)
    <> line <> indent 2 (pretty body)
  pretty (If c t f) =
    parens $
    "if" <+> pretty c <>
    align (vsep ["then:" <+> pretty t,
                 "else:" <+> pretty f])
  pretty (BinOp op x y) =
    parens $ sep [pretty op, pretty x, pretty y]

instance Typeable a => Typeable (Expr a) where
  typeOf (Var x) = typeOf x
  typeOf (Int _) = "Int"
  typeOf (Float _) = "Float"
  typeOf (Bool _) = "Bool"
  typeOf (Char _) = "Char"
  typeOf (String _) = "String"
  typeOf (Tuple xs) = TupleTy (map typeOf xs)
  typeOf (TupleAccess x i) =
    let TupleTy xs = typeOf x
    in fromMaybe (panic "out of bounds") (atMay xs i)
  typeOf Unit = "Unit"
  typeOf (Call fn _) =
    case typeOf fn of
      (FunTy _ ty) -> ty
      _            -> panic "(typeOf fn) should match (FunTy _ ty)"
  typeOf (Let _ e) = typeOf e
  typeOf (If _ e _) = typeOf e
  typeOf (BinOp _ x _) = typeOf x

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
  deriving (Eq, Show, Read)

instance Pretty Op where
  pretty Add      = "+"
  pretty Sub      = "-"
  pretty Mul      = "*"
  pretty Div      = "/"
  pretty FAdd     = "+."
  pretty FSub     = "-."
  pretty FMul     = "*."
  pretty FDiv     = "/."
  pretty Mod      = "%"
  pretty (Eq ty)  = "==" <> brackets (pretty ty)
  pretty (Neq ty) = "<>" <> brackets (pretty ty)
  pretty (Lt ty)  = "<" <> brackets (pretty ty)
  pretty (Gt ty)  = ">" <> brackets (pretty ty)
  pretty (Le ty)  = "<=" <> brackets (pretty ty)
  pretty (Ge ty)  = ">=" <> brackets (pretty ty)
  pretty And      = "&&"
  pretty Or       = "||"

data Decl a = FunDecs [FunDec a]
            | ValDec a (Expr a)
            | ExDec a Text
  deriving (Eq, Show, Read)

data FunDec a = FunDec a [a] (Expr a)
  deriving (Eq, Show, Read)

instance Pretty a => Pretty (Decl a) where
  pretty (ValDec name val) = "val" <+> pretty name <+> pretty val
  pretty (ExDec name orig) = "extern" <+> pretty name <+> pretty orig
  pretty (FunDecs fs)      = sep (map pretty fs)

instance Pretty a => Pretty (FunDec a) where
  pretty (FunDec name params body) =
    "fun" <+>
    (parens . sep $ pretty name : map pretty params)
    <> line <> indent 2 (pretty body)

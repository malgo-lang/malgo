{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Language.Malgo.HIR where

import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Text.PrettyPrint       hiding ((<>))

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

instance Outputable a => Outputable (Expr a) where
  ppr (Var x) = ppr x
  ppr (Int x) = integer x
  ppr (Float x) = double x
  ppr (Bool True) = text "true"
  ppr (Bool False) = text "false"
  ppr (Char x) = quotes $ char x
  ppr (String x) = doubleQuotes $ ppr x
  ppr (Tuple xs) =
    braces $ ppr xs
  ppr (TupleAccess xs i) =
    parens (text "." <+> ppr xs <+> int i)
  ppr Unit = text "{}"
  ppr (Call fn arg) = parens $ ppr fn <+> sep (map ppr arg)
  ppr (Let decl body) =
    parens $ text "let" <+> parens (ppr decl) $+$ nest (-1) (ppr body)
  ppr (If c t f) =
    parens $
    text "if" <+>
    ppr c $+$ text "then:" <+>
    nest 2 (ppr t) $+$ text "else:" <+> nest 2 (ppr f)
  ppr (BinOp op x y) = parens $ sep [ppr op, ppr x, ppr y]

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

instance Outputable Op where
  ppr Add      = text "+"
  ppr Sub      = text "-"
  ppr Mul      = text "*"
  ppr Div      = text "/"
  ppr FAdd     = text "+."
  ppr FSub     = text "-."
  ppr FMul     = text "*."
  ppr FDiv     = text "/."
  ppr Mod      = text "%"
  ppr (Eq ty)  = text "==" <> brackets (ppr ty)
  ppr (Neq ty) = text "<>" <> brackets (ppr ty)
  ppr (Lt ty)  = text "<" <> brackets (ppr ty)
  ppr (Gt ty)  = text ">" <> brackets (ppr ty)
  ppr (Le ty)  = text "<=" <> brackets (ppr ty)
  ppr (Ge ty)  = text ">=" <> brackets (ppr ty)
  ppr And      = text "&&"
  ppr Or       = text "||"

data Decl a = FunDecs [FunDec a]
            | ValDec a (Expr a)
            | ExDec a Text
  deriving (Eq, Show, Read)

data FunDec a = FunDec a [a] (Expr a)
  deriving (Eq, Show, Read)

instance Outputable a => Outputable (Decl a) where
  ppr (ValDec name val) = text "val" <+> ppr name <+> ppr val
  ppr (ExDec name orig) = text "extern" <+> ppr name <+> ppr orig
  ppr (FunDecs fs)      = sep (map ppr fs)

instance Outputable a => Outputable (FunDec a) where
  ppr (FunDec name params body) =
    text "fun" <+>
    (parens . sep $ ppr name : map ppr params) $+$
    nest 2 (ppr body)

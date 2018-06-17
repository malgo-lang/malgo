{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.IR.MIR where

import           Language.Malgo.IR.HIR     (Op (..))
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Language.Malgo.TypedID

data Expr a = Var a
            | Int Integer
            | Float Double
            | Bool Bool
            | Char Char
            | String Text
            | Unit
            | Tuple [a]
            | TupleAccess a Int
            | CallDir a [a]
            | CallCls a [a]
            | Let (Decl a) (Expr a)
            | If a (Expr a) (Expr a)
            | BinOp Op a a
    deriving (Show, Read)

instance Pretty a => Pretty (Expr a) where
  pretty (Var x) = pretty x
  pretty (Int x) = pretty x
  pretty (Float x) = pretty x
  pretty (Bool True) = "#t"
  pretty (Bool False) = "#f"
  pretty (Char x) = squotes $ pretty x
  pretty (String x) = dquotes $ pretty x
  pretty Unit = "{}"
  pretty (Tuple xs) =
    braces $ prettyList xs
  pretty (TupleAccess x i) =
    parens ("." <+> pretty x <+> pretty i)
  pretty (CallDir fn arg) = parens $ pretty fn <+> sep (map pretty arg)
  pretty (CallCls cls args) = braces $ pretty cls <+> sep (map pretty args)
  pretty (Let decl body) =
    parens $ "let" <+> parens (pretty decl)
    <> line <> indent 2 (pretty body)
  pretty (If c t f) =
    parens $
    "if" <+> pretty c <> line <>
    align (vsep ["then:" <+> pretty t, "else:" <+> pretty f])
  pretty (BinOp op x y) =
    parens $ sep [pretty op, pretty x, pretty y]

instance (Typeable a, Show a) => Typeable (Expr a) where
  typeOf (Var name) = typeOf name
  typeOf (Int _) = "Int"
  typeOf (Float _) = "Float"
  typeOf (Bool _) = "Bool"
  typeOf (Char _) = "Char"
  typeOf (String _) = "String"
  typeOf Unit = "Unit"
  typeOf (Tuple xs) = TupleTy (map typeOf xs)
  typeOf (TupleAccess x i) =
    let TupleTy xs = typeOf x
    in fromMaybe (panic "out of bounds") (atMay xs i)
  typeOf (CallDir name _) =
    case typeOf name of
      (FunTy _ ty) -> ty
      (ClsTy _ ty) -> ty
      _            -> panic $ show name <> "is not callable"
  typeOf (CallCls name _) =
    case typeOf name of
      (FunTy _ ty) -> ty
      (ClsTy _ ty) -> ty
      _            -> panic $ show name <> "is not callable"
  typeOf (Let _ e) = typeOf e
  typeOf (If _ t _) = typeOf t
  typeOf (BinOp op _ _) =
    case op of
      Add     -> "Int"
      Sub     -> "Int"
      Mul     -> "Int"
      Div     -> "Int"
      FAdd    -> "Float"
      FSub    -> "Float"
      FMul    -> "Float"
      FDiv    -> "Float"
      Mod     -> "Int"
      (Eq _)  -> "Bool"
      (Neq _) -> "Bool"
      (Lt _)  -> "Bool"
      (Gt _)  -> "Bool"
      (Le _)  -> "Bool"
      (Ge _)  -> "Bool"
      And     -> "Bool"
      Or      -> "Bool"

data FunDec a
  -- | FunDec function_name parameters captures body
  = FunDec a [a] [a] (Expr a)
    deriving (Show, Read)

instance Pretty a => Pretty (FunDec a) where
  pretty (FunDec name params capture body) =
    "fun" <+>
    (parens . sep $ pretty name : map pretty params) <+>
    (parens . sep $ map pretty capture) <> line <> indent 2 (pretty body)

data ExDec a
  -- | ExDec name_in_the_program original_name
  = ExDec a Text
    deriving (Show, Read)

instance Pretty a => Pretty (ExDec a) where
  pretty (ExDec name orig) = "extern" <+> pretty name <+> pretty orig

data Decl a = ValDec a (Expr a)
            -- | ClsDec closure_name function_name captures
            | ClsDec a a [a]
  deriving (Show, Read)

instance Pretty a => Pretty (Decl a) where
  pretty (ValDec name val) = "val" <+> pretty name <+> pretty val
  pretty (ClsDec name fn fv) =
    "closure" <+>
    pretty name <+> pretty fn <+> parens (sep $ map pretty fv)

data Program a = Program [FunDec a] [ExDec a] (Expr a) [TypedID]
  deriving (Show, Read)

instance Pretty a => Pretty (Program a) where
  pretty (Program t e m k) =
    "toplevel:" <> line <> cat (map pretty t)
    <> line <> "knowns:" <> line <> prettyList k
    <> line <> "extern:" <> line <> cat (map pretty e)
    <> line <> "main:" <> line <> pretty m

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Language.Malgo.MIR where

import           Language.Malgo.HIR     (Op (..))
import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Language.Malgo.TypedID
import           Text.PrettyPrint       hiding ((<>))

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

instance Outputable a => Outputable (Expr a) where
  ppr (Var x) = ppr x
  ppr (Int x) = integer x
  ppr (Float x) = double x
  ppr (Bool True) = "#t"
  ppr (Bool False) = "#f"
  ppr (Char x) = quotes $ char x
  ppr (String x) = doubleQuotes $ ppr x
  ppr Unit = "{}"
  ppr (Tuple xs) =
    braces $ ppr xs
  ppr (TupleAccess x i) =
    parens ("." <+> ppr x <+> int i)
  ppr (CallDir fn arg) = parens $ ppr fn <+> sep (map ppr arg)
  ppr (CallCls cls args) = braces $ ppr cls <+> sep (map ppr args)
  ppr (Let decl body) =
    parens $ "let" <+> parens (ppr decl) $+$ nest (-1) (ppr body)
  ppr (If c t f) =
    parens $
    "if" <+>
    ppr c $+$ "then:" <+>
    nest 2 (ppr t) $+$ "else:" <+> nest 2 (ppr f)
  ppr (BinOp op x y) = parens $ sep [ppr op, ppr x, ppr y]

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

instance Outputable a => Outputable (FunDec a) where
    ppr (FunDec name params capture body) =
        "fun" <+>
        (parens . sep $ ppr name : map ppr params) <+>
        (parens . sep $ map ppr capture) $+$ nest 2 (ppr body)

data ExDec a
  -- | ExDec name_in_the_program original_name
  = ExDec a Text
    deriving (Show, Read)

instance Outputable a => Outputable (ExDec a) where
    ppr (ExDec name orig) = "extern" <+> ppr name <+> ppr orig

data Decl a
    = ValDec a (Expr a)
    -- | ClsDec closure_name function_name captures
    | ClsDec a a [a]
    deriving (Show, Read)

instance Outputable a => Outputable (Decl a) where
    ppr (ValDec name val) = "val" <+> ppr name <+> ppr val
    ppr (ClsDec name fn fv) =
        "closure" <+>
        ppr name <+> ppr fn <+> parens (sep $ map ppr fv)

data Program a = Program [FunDec a] [ExDec a] (Expr a) [TypedID]
    deriving (Show, Read)

instance Outputable a => Outputable (Program a) where
    ppr (Program t e m k) =
        "toplevel:" $+$ cat (map ppr t)
        $+$ "knowns:" $+$ ppr k
        $+$ "extern:" $+$ cat (map ppr e)
        $+$ "main:" $+$ ppr m

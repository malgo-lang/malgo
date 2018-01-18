{-# LANGUAGE OverloadedStrings #-}

module Language.Malgo.MIR where

import           Language.Malgo.HIR   (Op (..))
import           Language.Malgo.Type
import           Language.Malgo.Utils
import           Text.PrettyPrint

data Expr a
    = Var a
    | Int Integer
    | Float Double
    | Bool Bool
    | Char Char
    | String String
    | Unit
    | CallDir a [a]
    | CallCls a [a]
    | Let (Decl a) (Expr a)
    | If a (Expr a) (Expr a)
    | BinOp Op a a
    deriving Show

instance PrettyPrint a => PrettyPrint (Expr a) where
    pretty (Var x) = pretty x
    pretty (Int x) = integer x
    pretty (Float x) = double x
    pretty (Bool True) = text "#t"
    pretty (Bool False) = text "#f"
    pretty (Char x) = quotes $ char x
    pretty (String x) = doubleQuotes $ text x
    pretty Unit = text "()"
    pretty (CallDir fn arg) = parens $ pretty fn <+> sep (map pretty arg)
    pretty (CallCls cls args) = braces $ pretty cls <+> sep (map pretty args)
    pretty (Let decl body) =
        parens $ text "let" <+> parens (pretty decl) $+$ nest (-1) (pretty body)
    pretty (If c t f) =
        parens $
        text "if" <+>
        pretty c $+$ text "then:" <+>
        nest 2 (pretty t) $+$ text "else:" <+> nest 2 (pretty f)
    pretty (BinOp op x y) = parens $ sep [pretty op, pretty x, pretty y]

instance (Typeable a, Show a) => Typeable (Expr a) where
    typeOf (Var name) = typeOf name
    typeOf (Int _) = "Int"
    typeOf (Float _) = "Float"
    typeOf (Bool _) = "Bool"
    typeOf (Char _) = "Char"
    typeOf (String _) = "String"
    typeOf Unit = "Unit"
    typeOf (CallDir name _) =
        case typeOf name of
            (FunTy _ ty) -> ty
            (ClsTy _ ty) -> ty
            (NameTy _)   -> error $ show name ++ "is not callable"
    typeOf (CallCls name _) =
        case typeOf name of
            (FunTy _ ty) -> ty
            (ClsTy _ ty) -> ty
            (NameTy _)   -> error $ show name ++ "is not callable"
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
    deriving Show

instance PrettyPrint a => PrettyPrint (FunDec a) where
    pretty (FunDec name params capture body) =
        text "fun" <+>
        (parens . sep $ pretty name : map pretty params) <+>
        (parens . sep $ map pretty capture) $+$ nest 2 (pretty body)

data ExDec a
  -- | ExDec name_in_the_program original_name
  = ExDec a String
    deriving (Show)

instance PrettyPrint a => PrettyPrint (ExDec a) where
    pretty (ExDec name orig) = text "extern" <+> pretty name <+> text orig

data Decl a
    = ValDec a
             (Expr a)
    -- | ClsDec closure_name function_name captures
    | ClsDec a a [a]
    deriving (Show)

instance PrettyPrint a => PrettyPrint (Decl a) where
    pretty (ValDec name val) = text "val" <+> pretty name <+> pretty val
    pretty (ClsDec name fn fv) =
        text "closure" <+>
        pretty name <+> pretty fn <+> parens (sep $ map pretty fv)

data Program a =
    Program [FunDec a]
            [ExDec a]
            (Expr a)
    deriving (Show)

instance PrettyPrint a => PrettyPrint (Program a) where
    pretty (Program t e m) =
        text "toplevel:" $+$ cat (map pretty t) $+$ text "extern:" $+$
        cat (map pretty e) $+$
        text "main:" $+$
        pretty m

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}

module Language.Malgo.Syntax where

import           Language.Malgo.Type
import           Language.Malgo.Prelude
import           Text.PrettyPrint

data Expr a
  -- | 変数参照
  = Var Info a
  -- | 32bit整数
  | Int Info Integer
  -- | 倍精度浮動小数点数
  | Float Info Double
  -- | 真(#t) 偽(#f)
  | Bool Info Bool
  -- | シングルクォートで囲まれた一文字
  | Char Info Char
  -- | ダブルクォートで囲まれた文字列
  | String Info Text
  -- | 空の値("()")
  | Unit Info
  -- | タプル
  | Tuple Info [Expr a]
  | TupleAccess Info (Expr a) Int
  -- | 関数呼び出し
  | Call Info (Expr a) [Expr a]
  -- | 無名関数
  | Fn Info [(a, Type)] (Expr a)
  -- | 連続した式(e1 ; e2)
  | Seq Info (Expr a) (Expr a)
  -- | let式
  | Let Info [Decl a] (Expr a)
  -- | if式
  | If Info (Expr a) (Expr a) (Expr a)
  -- | 中置演算子
  | BinOp Info Op (Expr a) (Expr a)
  deriving (Eq, Show, Read)

info :: Expr t -> Info
info (Var i _)           = i
info (Int i _)           = i
info (Float i _)         = i
info (Bool i _)          = i
info (Char i _)          = i
info (String i _)        = i
info (Tuple i _)         = i
info (TupleAccess i _ _) = i
info (Unit i)            = i
info (Call i _ _)        = i
info (Fn i _ _)          = i
info (Seq i _ _)         = i
info (Let i _ _)         = i
info (If i _ _ _)        = i
info (BinOp i _ _ _)     = i

instance PrettyPrint a => PrettyPrint (Expr a) where
  pretty (Var _ name) = pretty name
  pretty (Int _ x) = integer x
  pretty (Float _ x) = double x
  pretty (Bool _ True) = text "#t"
  pretty (Bool _ False) = text "#f"
  pretty (Char _ x) = quotes $ char x
  pretty (String _ x) = doubleQuotes $ pretty x
  pretty (Tuple _ xs) = braces $ sep (punctuate (text ",") (map pretty xs))
  pretty (TupleAccess _ e i) = parens (text "." <+> pretty e <+> int i)
  pretty (Unit _) = text "{}"
  pretty (Call _ fn arg) = parens $ pretty fn <+> sep (map pretty arg)
  pretty (Fn _ params body) =
    parens $ text "fn" <+> parens (sep (map (pretty . fst) params)) <+> pretty body
  pretty (Seq _ e1 e2) = parens $ text "seq" $+$ pretty e1 $+$ pretty e2
  pretty (Let _ decls body) =
    parens $
    text "let" <+> (parens . sep $ map pretty decls) $+$ nest 2 (pretty body)
  pretty (If _ c t f) =
    parens $ text "if" <+> pretty c $+$ nest 2 (pretty t) $+$ nest 2 (pretty f)
  pretty (BinOp _ op x y) = parens $ sep [pretty op, pretty x, pretty y]

-- | 中置演算子の種類を表すタグ
data Op
  = Add
  | Sub
  | Mul
  | Div
  | FAdd
  | FSub
  | FMul
  | FDiv
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
  deriving (Eq, Show, Read)

instance PrettyPrint Op where
  pretty Add  = text "+"
  pretty Sub  = text "-"
  pretty Mul  = text "*"
  pretty Div  = text "/"
  pretty FAdd = text "+."
  pretty FSub = text "-."
  pretty FMul = text "*."
  pretty FDiv = text "/."
  pretty Mod  = text "%"
  pretty Eq   = text "=="
  pretty Neq  = text "<>"
  pretty Lt   = text "<"
  pretty Gt   = text ">"
  pretty Le   = text "<="
  pretty Ge   = text ">="
  pretty And  = text "&&"
  pretty Or   = text "||"

data Decl a
  = FunDec Info a [(a, Type)] Type (Expr a)
  | ValDec Info a Type (Expr a)
  | ExDec Info a Type Text
  deriving (Eq, Show, Read)

instance PrettyPrint a => PrettyPrint (Decl a) where
  pretty (FunDec _ name params _ body) =
    parens $
    text "fun" <+>
    (parens . sep $ pretty name : map (\(n, _) -> pretty n) params) $+$
    nest 2 (pretty body)
  pretty (ValDec _ name _ val) =
    parens $ text "val" <+> pretty name <+> pretty val
  pretty (ExDec _ name _ orig) =
    parens $ text "extern" <+> pretty name <+> pretty orig

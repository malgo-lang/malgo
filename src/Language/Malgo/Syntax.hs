module Language.Malgo.Syntax where

import           Language.Malgo.Utils
import           Text.PrettyPrint

data Expr =
  -- | 変数参照
    Var Info Name
  -- | 32bit整数
  | Int Info Integer
  -- | 倍精度浮動小数点数
  | Float Info Double
  -- | 真(#t) 偽(#f)
  | Bool Info Bool
  -- | シングルクォートで囲まれた一文字
  | Char Info Char
  -- | ダブルクォートで囲まれた文字列
  | String Info String
  -- | 空の値("()")
  | Unit Info
  -- | 関数呼び出し
  | Call Info Name [Expr]
  -- | 連続した式(e1 ; e2)
  | Seq Info Expr Expr
  -- | let式
  | Let Info [Decl] Expr
  -- | if式
  | If Info Expr Expr Expr
  -- | 中置演算子
  | BinOp Info Op Expr Expr
  deriving (Eq, Show)

instance PrettyPrint Expr where
  pretty (Var _ name)     = pretty name
  pretty (Int _ x)         = integer x
  pretty (Float _ x)       = double x
  pretty (Bool _ True)     = text "#t"
  pretty (Bool _ False)    = text "#f"
  pretty (Char _ x)        = quotes $ char x
  pretty (String _ x)      = doubleQuotes $ text x
  pretty (Unit _)          = text "()"
  pretty (Call _ fn arg) = parens $ cat (pretty fn : map pretty arg)
  pretty (Seq _ e1 e2)    =  pretty e1 $+$ pretty e2
  pretty (Let _ decls body) =
    parens $ text "let" <+> (parens . sep $ map pretty decls)
    $+$ nest 2 (pretty body)
  pretty (If _ c t f) =
    parens $ text "if" <+> pretty c
    $+$ nest 2 (pretty t)
    $+$ nest 2 (pretty f)
  pretty (BinOp _ op x y) = parens (pretty op <+> pretty x <+> pretty y)

-- | Malgoの組み込みデータ型
data Type = NameTy Name
  deriving (Eq, Show)

instance PrettyPrint Type where
  pretty (NameTy n)          = pretty n
  -- pretty (TupleTy types)     = parens (cat $ punctuate (text ",") $ map pretty types)
  -- pretty (FunTy domTy codTy) = pretty domTy <+> text "->" <+> pretty codTy

data Decl = FunDec Info Name [(Name, Type)] Type Expr
          | ValDec Info Name Type Expr
  deriving (Eq, Show)

instance PrettyPrint Decl where
  pretty (FunDec _ name params retTy body) = parens $
    text "fun" <+> (parens . sep $ pretty name <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params)
    $+$ nest 2 (pretty body)
  pretty (ValDec _ name typ val) = parens $
    text "val" <+> pretty name <> colon <> pretty typ <+> pretty val

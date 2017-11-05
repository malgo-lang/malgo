module Language.Malgo.Syntax where

import           Language.Malgo.PrettyPrint
import           Language.Malgo.Types
import           Text.PrettyPrint

data Decl =
  -- | "var" <ident:1> ":" <type:2> "=" <const_expr:3>
    DefVar Info Name Type Const
  -- | "fun" <ident:1> "(" (<ident:3_0> ":" <type:3_1>)*:3 ")" ":" <type:2> "=" <expr:4>
  | DefFun Info Name Type [(Name, Type)] Expr
  -- | "extern var" <ident:1> ":" <type:2>
  | ExVar Info Name Type
  -- | "extern fun" <ident:1> "(" (<ident:3_0> ":" <type:3_1>)*:3 ")" ":" <type:2>
  | ExFun Info Name Type [(Name, Type)]
  deriving (Eq, Show)

instance PrettyPrint Decl where
  pretty (DefVar _ name typ val) =
    parens $ text "var" <+> pretty name <> colon <> pretty typ <+> pretty val
  pretty (DefFun _ fn retTy params body) =
    parens $ text "fun" <+> parens (sep (pretty fn <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params))
    $+$ nest 4 (pretty body)
  pretty (ExVar _ name typ) =
    parens $ text "extern var" <+> pretty name <> colon <> pretty typ
  pretty (ExFun _ fn retTy params) =
    parens $ text "extern fun" <+> parens (sep (pretty fn <> colon <> pretty retTy : map (\(n, t) -> pretty n <> colon <> pretty t) params))

data Const =
  -- | 32bit整数
    Int Info Integer
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
  -- | コンパイル時に計算される式
  | CBinOp Info Op Const Const
  deriving (Eq, Show)

instance PrettyPrint Const where
  pretty (Int _ x)         = integer x
  pretty (Float _ x)       = double x
  pretty (Bool _ True)     = text "#t"
  pretty (Bool _ False)    = text "#f"
  pretty (Char _ x)        = quotes $ char x
  pretty (String _ x)      = doubleQuotes $ text x
  pretty (Unit _)          = text "()"
  pretty (CBinOp _ op x y) = parens (pretty op <+> pretty x <+> pretty y)

data Expr =
  -- | 変数参照
    Var Info Name
  -- | 定数
  | Const Const
  -- | 関数呼び出し
  | Call Info Name [Expr]
  -- | 連続した式(e1 ; e2)
  | Seq Info Expr Expr
  -- | let式
  | Let Info Name Type Expr Expr
  -- | if式
  | If Info Expr Expr Expr
  -- | 中置演算子
  | BinOp Info Op Expr Expr
  deriving (Eq, Show)

instance PrettyPrint Expr where
  pretty (Var _ name)     = pretty name
  pretty (Const c)        = pretty c
  pretty (Call _ fn args) = parens . sep $ pretty fn : map pretty args
  pretty (Seq _ e1 e2)    =  pretty e1 $+$ pretty e2
  pretty (Let _ name typ val body) =
    parens $ text "let" <+> parens (pretty name <> colon <> pretty typ <+> pretty val)
    $+$ nest 2 (pretty body)
  pretty (If _ c t f) =
    parens $ text "if" <+> pretty c
    $+$ nest 2 (pretty t)
    $+$ nest 2 (pretty f)
  pretty (BinOp _ op x y) = parens (pretty op <+> pretty x <+> pretty y)

module Language.Malgo.Syntax where

import           Language.Malgo.Types

data Decl =
  -- ^ "def" <ident:1> ":" <type:2> "=" <const_expr:3>
    DefVar Info Name Type Const
  -- ^ "def" <ident:1> "(" (<ident:3_0> ":" <type:3_1>)*:3 ")" ":" <type:2> "=" <expr:4>
  | DefFun Info Name Type [(Name, Type)] Expr
  -- ^ "extern" <ident:1> ":" <type:2>
  | ExVar Info Name Type
  -- ^ "extern" <ident:1> "(" (<ident:3_0> ":" <type:3_1>)*:3 ")" ":" <type:2>
  | ExFun Info Name Type [(Name, Type)]
  deriving (Eq, Show)

data Const =
  -- ^ 32bit整数
    Int Info Integer
  -- ^ 倍精度浮動小数点数
  | Float Info Double
  -- ^ 真(#t) 偽(#f)
  | Bool Info Bool
  -- ^ シングルクォートで囲まれた一文字
  | Char Info Char
  -- ^ ダブルクォートで囲まれた文字列
  | String Info String
  -- ^ 空の値("()")
  | Unit Info
  -- ^ コンパイル時に計算される式
  | CBinOp Info Op Const Const
  deriving (Eq, Show)

data Expr =
  -- ^ 変数参照
    Var Info Name
  -- ^ 定数
  | Const Const
  -- ^ 関数呼び出し
  | Call Info Name [Expr]
  -- ^ 連続した式(e1 ; e2)
  | Seq Info Expr Expr
  -- ^ let式
  | Let Info Name Type Expr Expr
  -- ^ if式
  | If Info Expr Expr Expr
  -- ^ 中置演算子
  | BinOp Info Op Expr Expr
  deriving (Eq, Show)

{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Syntax where

import           Data.String
import           Language.Malgo.Utils
import           Text.PrettyPrint

data Expr a =
  -- | 変数参照
    Var Info a
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
  | Call Info a -- (Expr a) -- そのうち任意の関数型をもつ式を呼び出し可能にする
    [Expr a]
  -- | 連続した式(e1 ; e2)
  | Seq Info (Expr a) (Expr a)
  -- | let式
  | Let Info [Decl a] (Expr a)
  -- | if式
  | If Info (Expr a) (Expr a) (Expr a)
  -- | 中置演算子
  | BinOp Info Op (Expr a) (Expr a)
  deriving (Eq, Show)

instance PrettyPrint a => PrettyPrint (Expr a) where
  pretty (Var _ name)     = pretty name
  pretty (Int _ x)         = integer x
  pretty (Float _ x)       = double x
  pretty (Bool _ True)     = text "#t"
  pretty (Bool _ False)    = text "#f"
  pretty (Char _ x)        = quotes $ char x
  pretty (String _ x)      = doubleQuotes $ text x
  pretty (Unit _)          = text "()"
  pretty (Call _ fn arg) = parens $ pretty fn <+> sep (map pretty arg)
  pretty (Seq _ e1 e2)    =  parens $ text "seq" $+$ pretty e1 $+$ pretty e2
  pretty (Let _ decls body) =
    parens $ text "let" <+> (parens . sep $ map pretty decls)
    $+$ nest 2 (pretty body)
  pretty (If _ c t f) =
    parens $ text "if" <+> pretty c
    $+$ nest 2 (pretty t)
    $+$ nest 2 (pretty f)
  pretty (BinOp _ op x y) = parens $ sep [pretty op, pretty x, pretty y]

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
        | Eq
        | Neq
        | Lt
        | Gt
        | Le
        | Ge
        | And
        | Or
  deriving (Eq, Show)

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

-- | Malgoの組み込みデータ型
data Type = NameTy Name
          -- | TupleTy [Type]
          | FunTy { _params :: [Type]
                  , _ret    :: Type
                  }
          | ClsTy { _params   :: [Type]
                  , _ret      :: Type
                  , _captures :: [Type]
                  }
  deriving (Eq, Show)

instance PrettyPrint Type where
  pretty (NameTy n)          = pretty n
  -- pretty (TupleTy types)     = parens (cat $ punctuate (text ",") $ map pretty types)
  pretty (FunTy param ret) =
    parens (cat (punctuate (text ",") (map pretty param))
    <+> text "->" <+> pretty ret)
  pretty (ClsTy param ret cap) = braces
    (parens (cat (punctuate (text ",") (map pretty param))
             <+> text "->" <+> pretty ret))
    <+> brackets (sep (map pretty cap))

instance IsString Type where
  fromString name = NameTy $ fromString name

data Decl a = FunDec Info a [(a, Type)] Type (Expr a)
            | ValDec Info a Type (Expr a)
            | ExDec Info a Type String
  deriving (Eq, Show)

instance PrettyPrint a => PrettyPrint (Decl a) where
  pretty (FunDec _ name params _ body) = parens $
    text "fun" <+> (parens . sep $ pretty name -- <> colon <> pretty retTy
                    : map (\(n, _) -> pretty n -- <> colon <> pretty t
                          ) params)
    $+$ nest 2 (pretty body)
  pretty (ValDec _ name _ val) = parens $
    text "val" <+> pretty name -- <> colon <> pretty typ
    <+> pretty val
  pretty (ExDec _ name _ orig) = parens $
    text "extern" <+> pretty name -- <> colon <> pretty typ
    <+> text orig

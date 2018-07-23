{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.IR.Core where

import           Language.Malgo.FrontEnd.Info
import           RIO
import Data.Text.Prettyprint.Doc

data Expr t a
  -- | 変数
  = Var Info a
  -- | 32bit整数
  | Int Info Integer
  -- | 倍精度浮動小数点数
  | Float Info Double
  -- | 真偽値
  | Bool Info Bool
  -- | シングルクォートで囲まれた文字
  | Char Info Char
  -- | ダブルクォートで囲まれた文字列
  | String Info Text
  -- | タプル
  | Tuple Info [Expr t a]
  -- | タプルの要素の取り出し
  | TupleAccess Info (Expr t a) Int
  -- | 関数適用
  | App Info (Maybe (Type t)) (Expr t a) (Expr t a)
  -- | 無名関数
  | Fn Info [(a, Type t)] (Expr t a)
  -- | 連続した式(e1; e2)
  | Seq Info (Expr t a) (Expr t a)
  -- | let式
  | Let Info (Bind t a) (Expr t a)
  -- | if式
  | If Info (Expr t a) (Expr t a) (Expr t a)
  -- | 組み込み関数
  | Prim a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Show t, Show a, Pretty t, Pretty a) => Pretty (Expr t a) where
  pretty (Var _ a) = pretty a
  pretty (Int _ i) = pretty i
  pretty (Float _ f) = pretty f
  pretty (Bool _ True) = "true"
  pretty (Bool _ False) = "false"
  pretty (Char _ c) = squotes $ pretty c
  pretty (String _ s) = dquotes $ pretty s
  pretty (Tuple _ xs) = braces (sep (punctuate "," (map pretty xs)))
  pretty (TupleAccess _ x i) = pretty x <> brackets (pretty i)
  -- TODO: 左結合な表示
  pretty (App _ Nothing fn arg) = parens (pretty fn <+> pretty arg)
  pretty (App _ (Just ty) fn arg) =
    parens (pretty fn <+> ("<" <> pretty ty <> ">") <+> pretty arg)
  pretty (Fn _ params body) =
    parens ("fn" <+> parens (sep (map pretty' params)) <+> pretty body)
    where pretty' (a, t) = parens $ pretty a <> ":" <> pretty t
  pretty (Seq _ e1 e2) = pretty e1 <> ";" <> line <> pretty e2
  pretty (Let _ bind body) =
    parens $ "let" <+> pretty bind <> line <> indent 1 (pretty body)
  pretty (If _ c t f) =
    parens $ "if" <+> pretty c
    <> line <> indent 2 (pretty t)
    <> line <> indent 2 (pretty f)
  pretty (Prim a) = "#" <> pretty a

data Type t = IdTy t
            | Type t :-> Type t
            | TupleTy [t]
            | PolyTy [t] (Type t)
  deriving (Show, Eq, Ord)

instance Show t => Pretty (Type t)

data Bind t a = NonRec Info (BindField t a)
              | Rec Info [BindField t a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Show t, Show a) => Pretty (Bind t a)

data BindField t a = BindField a (Maybe (Type t)) (Expr t a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Show t, Show a) => Pretty (BindField t a)

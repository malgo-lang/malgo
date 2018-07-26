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
import           Language.Malgo.Pretty

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

instance (Pretty t, Pretty a) => Pretty (Expr t a) where
  pPrint (Var _ a) = pPrint a
  pPrint (Int _ i) = pPrint i
  pPrint (Float _ f) = pPrint f
  pPrint (Bool _ True) = "true"
  pPrint (Bool _ False) = "false"
  pPrint (Char _ c) = quotes $ pPrint c
  pPrint (String _ s) = doubleQuotes $ pPrint s
  pPrint (Tuple _ xs) = braces (sep (punctuate "," (map pPrint xs)))
  pPrint (TupleAccess _ x i) = pPrint x <> brackets (pPrint i)
  -- TODO: 左結合な表示
  pPrint (App _ Nothing fn arg) = parens (pPrint fn <+> pPrint arg)
  pPrint (App _ (Just ty) fn arg) =
    parens (pPrint fn <+> ("<" <> pPrint ty <> ">") <+> pPrint arg)
  pPrint (Fn _ params body) =
    parens ("fn" <+> parens (sep (map pPrint' params)) <+> pPrint body)
    where pPrint' (a, t) = parens $ pPrint a <> ":" <> pPrint t
  pPrint (Seq _ e1 e2) = pPrint e1 <> ";" $+$ pPrint e2
  pPrint (Let _ bind body) =
    parens $ "let" <+> pPrint bind $+$ nest 1 (pPrint body)
  pPrint (If _ c t f) =
    parens $ "if" <+> pPrint c
    $+$ nest 2 (pPrint t)
    $+$ nest 2 (pPrint f)
  pPrint (Prim a) = "#" <> pPrint a

data Type t = IdTy t
            | Type t :-> Type t
            | TupleTy [t]
            | PolyTy [t] (Type t)
  deriving (Show, Eq, Ord)

instance Pretty (Type t)

data Bind t a = NonRec Info (BindField t a)
              | Rec Info [BindField t a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Pretty (Bind t a)

data BindField t a = BindField a (Maybe (Type t)) (Expr t a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Pretty (BindField t a)

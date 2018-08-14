{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.IR.Core where

import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.Pretty
import           Universum                   hiding (Type)

data Expr t a
  -- | 変数
  = Var SrcSpan a
  -- | 32bit整数
  | Int SrcSpan Integer
  -- | 倍精度浮動小数点数
  | Float SrcSpan Double
  -- | 真偽値
  | Bool SrcSpan Bool
  -- | シングルクォートで囲まれた文字
  | Char SrcSpan Char
  -- | ダブルクォートで囲まれた文字列
  | String SrcSpan Text
  -- | タプル
  | Tuple SrcSpan [Expr t a]
  -- | タプルの要素の取り出し
  | TupleAccess SrcSpan (Expr t a) Int
  -- | 関数適用
  | App SrcSpan (Expr t a) (Expr t a)
  -- | 無名関数
  | Fn SrcSpan [(a, Type t)] (Expr t a)
  -- | let式
  | Let SrcSpan (Bind t a) (Expr t a)
  -- | if式
  | If SrcSpan (Expr t a) (Expr t a) (Expr t a)
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
  pPrint (App _ fn arg) = parens (pPrint fn <+> pPrint arg)
  pPrint (Fn _ params body) =
    parens ("fn" <+> parens (sep (map pPrint' params)) <+> pPrint body)
    where pPrint' (a, t) = parens $ pPrint a <> ":" <> pPrint t
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
            | AnswerTy
            --  | PolyTy [t] (Type t)
  deriving (Show, Eq, Ord)

infixr 7 :->
instance Pretty t => Pretty (Type t) where
  pPrintPrec l p (IdTy a) = pPrintPrec l p a
  pPrintPrec l _ (TupleTy xs) =
    braces $ fsep $ punctuate comma (map pPrint0 xs)
    where pPrint0 = pPrintPrec l 0
  pPrintPrec l p (t1 :-> t2) =
    maybeParens (p > 7) $ pPrintPrec l 8 t1 <+> ":->" <+> pPrintPrec l 8 t2
  pPrintPrec _ _ AnswerTy = "Answer"

data Bind t a = NonRec SrcSpan (BindField t a)
              | Rec SrcSpan [BindField t a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Pretty t, Pretty a) => Pretty (Bind t a) where
  pPrint (NonRec _ bf) = parens $ pPrint bf
  pPrint (Rec _ bfs)   = hsep $ map (parens . pPrint) bfs

data BindField t a = BindField a (Maybe (Type t)) (Expr t a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Pretty t, Pretty a) => Pretty (BindField t a) where
  pPrint (BindField name mty expr) =
    pPrint name <+> maybeToMonoid
    ((\ty -> colon <+> pPrint ty) <$> mty) <+> "="
    $+$ nest 2 (pPrint expr)

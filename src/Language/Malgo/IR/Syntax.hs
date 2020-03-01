{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Malgoの抽象構文木の定義
module Language.Malgo.IR.Syntax
  ( module Export
  , Decl(..)
  , Expr(..)
  , Pat(..)
  , info
  )
where

import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.TypeRep.Type

import           Language.Malgo.IR.Op          as Export

import           Language.Malgo.FrontEnd.Info

import           Text.PrettyPrint.HughesPJClass ( quotes
                                                , doubleQuotes
                                                , braces
                                                , sep
                                                , punctuate
                                                , parens
                                                , brackets
                                                , ($+$)
                                                )

-- | 式。
-- 型変数aは識別子の型を表す。ParserはExpr Stringを生成する。
data Expr a
  = Var Info a -- ^ 変数参照
  | Int Info Integer -- ^ 64bit整数
  | Float Info Double -- ^ 倍精度浮動小数点数
  | Bool Info Bool -- ^ 真偽値
  | Char Info Char -- ^ 文字リテラル。シングルクォート(')で囲まれた一文字
  | String Info String -- ^ 文字列リテラル。ダブルクォートで囲まれた文字列
  | Tuple Info [Expr a] -- ^ タプル
  | Array Info (NonEmpty (Expr a)) -- ^ 配列リテラル
  | MakeArray -- ^ 配列の作成
    Info
    (Expr a) -- ^ 初期値 
    (Expr a) -- ^ 長さ
  | ArrayRead -- ^ 配列の要素へのアクセス
    Info
    (Expr a) -- ^ 配列
    (Expr a) -- ^ 要素のインデックス
  | ArrayWrite -- ^ 配列の要素の書き込み
    Info
    (Expr a) -- ^ 配列
    (Expr a) -- ^ 要素のインデックス
    (Expr a) -- ^ 書き込む式
  | Call -- ^ 関数呼び出し
    Info
    (Expr a) -- ^ 関数
    [Expr a] -- ^ 引数のリスト
  | Fn -- ^ 無名関数
    Info
    [(a, Maybe Type)] -- ^ 仮引数とその型のリスト
    (Expr a) -- ^ 関数本体
  | Seq -- ^ 連続した式(e1; e2)
    Info
    (Expr a) -- ^ 先に実行される式
    (Expr a) -- ^ 後に実行される式
  | Let -- ^ 変数, 関数, 外部関数の定義
    Info
    (Decl a) -- ^ 定義
    (Expr a) -- ^ 定義の元で実行される式
  | If -- ^ if式
    Info
    (Expr a) -- ^ 条件
    (Expr a) -- ^ 真のときの式
    (Expr a) -- ^ 偽のときの式
  | BinOp -- ^ 中置演算子
    Info
    Op -- ^ 演算子
    (Expr a) -- ^ 左辺
    (Expr a) -- ^ 右辺
  | Match -- ^ パターンマッチ
    Info
    (Expr a) -- ^ 対象とある値
    (NonEmpty (Pat a, Expr a)) -- ^ パターンとマッチしたときの式のリスト
  deriving stock (Eq, Show, Read, Functor, Foldable, Traversable)

-- | Exprからソースコード上の位置情報を取り出すための補助関数
info :: Expr t -> Info
info (Var    i _        ) = i
info (Int    i _        ) = i
info (Float  i _        ) = i
info (Bool   i _        ) = i
info (Char   i _        ) = i
info (String i _        ) = i
info (Tuple  i _        ) = i
info (Array  i _        ) = i
info (MakeArray i _ _   ) = i
info (ArrayRead i _ _   ) = i
info (ArrayWrite i _ _ _) = i
info (Call i _ _        ) = i
info (Fn   i _ _        ) = i
info (Seq  i _ _        ) = i
info (Let  i _ _        ) = i
info (If    i _ _ _     ) = i
info (BinOp i _ _ _     ) = i
info (Match i _ _       ) = i

instance Pretty a => Pretty (Expr a) where
  pPrint (Var    _ name        ) = pPrint name
  pPrint (Int    _ x           ) = pPrint x
  pPrint (Float  _ x           ) = pPrint x
  pPrint (Bool   _ True        ) = "true"
  pPrint (Bool   _ False       ) = "false"
  pPrint (Char   _ x           ) = quotes $ pPrint x
  pPrint (String _ x           ) = doubleQuotes $ pPrint x
  pPrint (Tuple  _ xs          ) = braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (Array  _ xs          ) = brackets $ sep $ punctuate "," $ toList $ fmap pPrint xs
  pPrint (MakeArray _ init size) = parens $ "array" <+> pPrint init <+> pPrint size
  pPrint (ArrayRead _ arr  ix  ) = pPrint arr <> brackets (pPrint ix)
  pPrint (ArrayWrite _ arr ix val) =
    parens $ "<-" <+> (pPrint arr <> brackets (pPrint ix)) <+> pPrint val
  pPrint (Call _ fn arg) = parens $ pPrint fn <+> sep (map pPrint arg)
  pPrint (Fn _ params body) =
    parens $ "fn" <+> parens (sep $ punctuate "," (map (pPrint . fst) params)) <+> pPrint body
  pPrint (Seq _ e1   e2  ) = parens $ "seq" <+> (pPrint e1 $+$ pPrint e2)
  pPrint (Let _ decl body) = parens $ "let" <+> pPrint decl $+$ pPrint body
  pPrint (If    _ c  t f ) = parens $ "if" <+> pPrint c $+$ pPrint t $+$ pPrint f
  pPrint (BinOp _ op x y ) = parens $ sep [pPrint op, pPrint x, pPrint y]
  pPrint (Match _ s cs   ) = parens $ "match" <+> pPrint s $+$ sep
    (punctuate "|" (toList $ fmap pPrintClause cs))
    where pPrintClause (p, e) = pPrint p <+> "=>" <+> pPrint e

-- | パターン
data Pat a = VarP a -- ^ 変数パターン
           | TupleP [Pat a] -- ^ タプルパターン
  deriving stock (Eq, Show, Read, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Pat a) where
  pPrint (VarP   x ) = pPrint x
  pPrint (TupleP xs) = braces $ sep $ punctuate "," $ map pPrint xs

instance HasType a => HasType (Pat a) where
  typeOf (VarP   x ) = typeOf x
  typeOf (TupleP xs) = TyApp TupleC $ map typeOf xs

-- | 変数定義、関数定義、外部関数定義
data Decl a
  = FunDec [(Info, a, [(a, Maybe Type)], Maybe Type, Expr a)] -- ^ 関数定義。相互再帰しうる関数定義のリスト
  | ValDec Info a (Maybe Type) (Expr a) -- ^ 変数定義
  | ExDec Info a Type String -- ^ 外部関数定義
  deriving stock (Eq, Show, Read, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Decl a) where
  pPrint (FunDec fs) = sep $ map pp fs
   where
    pp (_, name, params, _, body) =
      parens
        $   "fun"
        <+> (parens . sep $ pPrint name : map (\(n, _) -> pPrint n) params)
        $+$ pPrint body
  pPrint (ValDec _ name _ val ) = parens $ "val" <+> pPrint name <+> pPrint val
  pPrint (ExDec  _ name _ orig) = parens $ "extern" <+> pPrint name <+> pPrint orig

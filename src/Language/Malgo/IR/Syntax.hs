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
  , position
  )
where

import           Language.Malgo.Prelude  hiding ( ix
                                                , op
                                                )
import           Language.Malgo.Pretty

import           Language.Malgo.TypeRep.Type
import           Language.Malgo.TypeRep.SType

import           Language.Malgo.IR.Op          as Export

import           Text.PrettyPrint.HughesPJClass ( quotes
                                                , doubleQuotes
                                                , braces
                                                , sep
                                                , punctuate
                                                , parens
                                                , brackets
                                                , ($+$)
                                                )
import           Text.Parsec.Pos                ( SourcePos )

-- | 式。
-- 型変数aは識別子の型を表す。ParserはExpr Stringを生成する。
data Expr a
  = Var SourcePos a -- ^ 変数参照
  | Int SourcePos Integer -- ^ 64bit整数
  | Float SourcePos Double -- ^ 倍精度浮動小数点数
  | Bool SourcePos Bool -- ^ 真偽値
  | Char SourcePos Char -- ^ 文字リテラル。シングルクォート(')で囲まれた一文字
  | String SourcePos String -- ^ 文字列リテラル。ダブルクォートで囲まれた文字列
  | Tuple SourcePos [Expr a] -- ^ タプル
  | Array SourcePos (NonEmpty (Expr a)) -- ^ 配列リテラル
  | MakeArray -- ^ 配列の作成
    SourcePos
    (Expr a) -- ^ 初期値 
    (Expr a) -- ^ 長さ
  | ArrayRead -- ^ 配列の要素へのアクセス
    SourcePos
    (Expr a) -- ^ 配列
    (Expr a) -- ^ 要素のインデックス
  | ArrayWrite -- ^ 配列の要素の書き込み
    SourcePos
    (Expr a) -- ^ 配列
    (Expr a) -- ^ 要素のインデックス
    (Expr a) -- ^ 書き込む式
  | Call -- ^ 関数呼び出し
    SourcePos
    (Expr a) -- ^ 関数
    [Expr a] -- ^ 引数のリスト
  | Fn -- ^ 無名関数
    SourcePos
    [(a, Maybe (SType a))] -- ^ 仮引数とその型のリスト
    (Expr a) -- ^ 関数本体
  | Seq -- ^ 連続した式(e1; e2)
    SourcePos
    (Expr a) -- ^ 先に実行される式
    (Expr a) -- ^ 後に実行される式
  | Let -- ^ 変数, 関数, 外部関数の定義
    SourcePos
    (Decl a) -- ^ 定義
    (Expr a) -- ^ 定義の元で実行される式
  | If -- ^ if式
    SourcePos
    (Expr a) -- ^ 条件
    (Expr a) -- ^ 真のときの式
    (Expr a) -- ^ 偽のときの式
  | BinOp -- ^ 中置演算子
    SourcePos
    Op -- ^ 演算子
    (Expr a) -- ^ 左辺
    (Expr a) -- ^ 右辺
  | Match -- ^ パターンマッチ
    SourcePos
    (Expr a) -- ^ 対象とある値
    (NonEmpty (Pat a, Expr a)) -- ^ パターンとマッチしたときの式のリスト
  deriving stock (Eq, Show, Functor)

-- | Exprからソースコード上の位置情報を取り出すための補助関数
position :: Expr t -> SourcePos
position (Var    i _        ) = i
position (Int    i _        ) = i
position (Float  i _        ) = i
position (Bool   i _        ) = i
position (Char   i _        ) = i
position (String i _        ) = i
position (Tuple  i _        ) = i
position (Array  i _        ) = i
position (MakeArray i _ _   ) = i
position (ArrayRead i _ _   ) = i
position (ArrayWrite i _ _ _) = i
position (Call i _ _        ) = i
position (Fn   i _ _        ) = i
position (Seq  i _ _        ) = i
position (Let  i _ _        ) = i
position (If    i _ _ _     ) = i
position (BinOp i _ _ _     ) = i
position (Match i _ _       ) = i

instance Pretty a => Pretty (Expr a) where
  pPrint (Var    _ name     ) = pPrint name
  pPrint (Int    _ x        ) = pPrint x
  pPrint (Float  _ x        ) = pPrint x
  pPrint (Bool   _ True     ) = "true"
  pPrint (Bool   _ False    ) = "false"
  pPrint (Char   _ x        ) = quotes $ pPrint x
  pPrint (String _ x        ) = doubleQuotes $ pPrint x
  pPrint (Tuple  _ xs       ) = braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (Array  _ xs       ) = brackets $ sep $ punctuate "," $ toList $ fmap pPrint xs
  pPrint (MakeArray _ x   n ) = parens $ "array" <+> pPrint x <+> pPrint n
  pPrint (ArrayRead _ arr ix) = pPrint arr <> brackets (pPrint ix)
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
  deriving stock (Eq, Show, Functor)

instance Pretty a => Pretty (Pat a) where
  pPrint (VarP   x ) = pPrint x
  pPrint (TupleP xs) = braces $ sep $ punctuate "," $ map pPrint xs

instance HasType a => HasType (Pat a) where
  typeOf (VarP   x ) = typeOf x
  typeOf (TupleP xs) = TyApp TupleC $ map typeOf xs

-- | 変数定義、関数定義、外部関数定義
data Decl a
  = FunDec [(SourcePos, a, [(a, Maybe (SType a))], Maybe (SType a), Expr a)] -- ^ 関数定義。相互再帰しうる関数定義のリスト
  | ValDec SourcePos a (Maybe (SType a)) (Expr a) -- ^ 変数定義
  | ExDec SourcePos a (SType a) String -- ^ 外部関数定義
  deriving stock (Eq, Show, Functor)

instance Pretty a => Pretty (Decl a) where
  pPrint (FunDec fs) = sep $ map pp fs
   where
    pp (_, name, params, ret, body) =
      parens
        $   "fun"
        <+> ( parens
            . sep
            $ pPrint name
            : map (\(n, t) -> pPrint n <> maybe mempty ((":" <>) . pPrint) t) params
            )
        <>  maybe mempty ((":" <>) . pPrint) ret
        $+$ pPrint body
  pPrint (ValDec _ name t val) =
    parens $ "val" <+> pPrint name <> maybe mempty ((":" <>) . pPrint) t <+> pPrint val
  pPrint (ExDec _ name t orig) =
    parens $ "extern" <+> pPrint name <> ":" <> pPrint t <+> pPrint orig

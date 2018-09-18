module Language.Malgo.IR.AST where

import           Language.Malgo.FrontEnd.Loc
import           Universum

data Expr a = Var SrcSpan a
            | Literal SrcSpan Literal
            | Record SrcSpan [(a, Expr a)]
            | Variant SrcSpan a (Expr a) SType
            | Let [Bind a] (Expr a)
            | Apply (Expr a) (Expr a)
            | Case SrcSpan (Expr a) [Clause a]
            | Fn SrcSpan [(a, Maybe SType)] (Expr a)

{- # Function literal transformation
(fun (x:a) (y:b) (z:c) -> e:d) : a -> b -> c -> d
=> Fn SrcSpan [(x, a), (y, b), (z, c)] e
=> Fn SrcSpan [(x, a)] (Fn SrcSpan [(y, b)] (Fn SrcSpan [(z, c)] e))
-}

data Literal = Int Integer
             | Float Double
             | Bool Bool
             | Char Char

data Bind a = NonRec SrcSpan a (Maybe SType) (Expr a)
            | Rec SrcSpan a (Maybe SType) [a] (Expr a)

{- # Rec transformation
rec f x y : a -> b -> c = e
=> Rec SrcSpan f (a -> b -> c) [x, y] e
=> Rec SrcSpan f (a -> b -> c) [] (Fn SrcSpan [(x, a)] (Fn SrcSpan [(y, b)] e))
-}

data Clause a = VariantPat SrcSpan a a SType (Expr a)
              | BoolPat SrcSpan Bool (Expr a)
              | VarPat SrcSpan a (Expr a)

-- | トップレベル宣言
data Decl a = ScDef SrcSpan a [a] (Expr a) -- ^ 環境を持たない関数（定数）宣言
            | ScAnn SrcSpan a SType -- ^ 関数（定数）の型宣言
            | AliasDef SrcSpan SType [Text] SType -- ^ 型の別名定義
            | TypeDef SrcSpan SType [Text] SType -- ^ 新しい型の定義


-- | ソースコード上での型の表現
data SType = STyApp STyCon [SType]
           | STyVar Text

{- # SType vs Type
型検査時にLanguage.Malgo.Type.Typeへ翻訳される．
Forallは型検査の過程で自動生成される．
-}

data STyCon = SimpleC Text
            | SRecordC [(Text, SType)]
            | SVariantC [(Text, SType)]

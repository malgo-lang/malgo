module Language.Malgo.IR.AST where

import           Language.Malgo.FrontEnd.Loc
import           Universum

data Expr a = Var SrcSpan a
            | Literal SrcSpan Literal
            | Record SrcSpan [(a, Expr a)]
            | Variant SrcSpan a (Expr a) SType
            | Let (Bind a)
            | Case SrcSpan (Expr a) [Clause a]
            | Fn SrcSpan [(a, Maybe SType)] (Expr a)

data Literal = Int Integer
             | Float Double
             | Bool Bool
             | Char Char

data Bind a = NonRec SrcSpan a (Maybe SType) (Expr a)
            | Rec [(SrcSpan, a, Maybe SType, [a], Expr a)]

data Clause a = VariantPat SrcSpan a a SType (Expr a)
              | BoolPat SrcSpan Bool (Expr a)

-- | トップレベル宣言
data Decl a = ScDef SrcSpan a [a] (Expr a) -- ^ 環境を持たない関数（定数）宣言
            | ScAnn SrcSpan a SType -- ^ 関数（定数）の型宣言
            | TyDef SrcSpan SType [Text] SType -- ^ 型の別名定義
            | NewTyDef SrcSpan SType [Text] SType -- ^ 新しい型の定義


-- | ソースコード上での型の表現
--
--   型検査時にLanguage.Malgo.Type.Typeへ翻訳される．
--
--   Forallは型検査の過程で自動生成される．
data SType = STyApp STyCon [SType]
           | STyVar Text

data STyCon = SimpleC Text
            | SRecordC [(Text, SType)]
            | SVariantC [(Text, SType)]

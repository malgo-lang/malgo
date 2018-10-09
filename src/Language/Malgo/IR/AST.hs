{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.IR.AST where

import           Language.Malgo.FrontEnd.Loc
import           Universum

data Expr a = Var SrcSpan a
            | Literal SrcSpan Literal
            | Record SrcSpan [(Text, Expr a)]
            | Variant SrcSpan Text (Expr a) [(Text, SType a)]
            | Access SrcSpan (Expr a) Text
            | Let SrcSpan (Bind a) (Expr a)
            | Apply SrcSpan (Expr a) (Expr a)
            | Case SrcSpan (Expr a) [Clause a]
            | Fn SrcSpan [(a, Maybe (SType a))] (Expr a)
  deriving (Eq, Show, Generic)

instance SrcInfo (Expr a) where
  srcSpan (Var ss _)         = ss
  srcSpan (Literal ss _)     = ss
  srcSpan (Record ss _)      = ss
  srcSpan (Variant ss _ _ _) = ss
  srcSpan (Access ss _ _)    = ss
  srcSpan (Let ss _ _)       = ss
  srcSpan (Apply ss _ _)     = ss
  srcSpan (Case ss _ _)      = ss
  srcSpan (Fn ss _ _)        = ss

{- # Function literal transformation
(fun (x:a) (y:b) (z:c) -> e:d) : a -> b -> c -> d
=> Fn SrcSpan [(x, a), (y, b), (z, c)] e
=> Fn SrcSpan [(x, a)] (Fn SrcSpan [(y, b)] (Fn SrcSpan [(z, c)] e))
-}

extendFn :: Expr a -> Expr a
extendFn (Fn ss (p : ps) e) = Fn ss [p] (extendFn $ Fn ss ps e)
extendFn e                  = e

data Literal = Int Integer
             | Float Double
             | Bool Bool
             | Char Char
  deriving (Eq, Show, Generic)

data Bind a = NonRec SrcSpan a (Maybe (SType a)) (Expr a)
            | Rec [(SrcSpan, a, Maybe (SType a), [a], Expr a)]
  deriving (Eq, Show, Generic)

instance SrcInfo (Bind a) where
  srcSpan (NonRec ss _ _ _) = ss
  srcSpan (Rec xs) =
    foldl1 (curry srcSpan) $ map (view _1) xs

{- # Rec transformation
rec f x y : a -> b -> c = e
=> Rec [(SrcSpan, f, a -> b -> c, [x, y], e)]
=> Rec [(SrcSpan, f, a -> b -> c, [], Fn SrcSpan [(x, Nothing)] (Fn SrcSpan [(y, Nothing)] e))]
-}

extendRec :: Bind a -> Bind a
extendRec (Rec bs) = Rec $ map extendRec' bs
 where
  extendRec' b@(_, _, _, [], _) = b
  extendRec' (ss, f, ty, xs, e) =
    ( ss
    , f
    , ty
    , []
    , foldr (\x -> Fn ss [x]) e $ reverse $ zip xs $ repeat Nothing
    )
extendRec b = b

data Clause a = VariantPat SrcSpan Text a [(Text, SType a)] (Expr a)
              | BoolPat SrcSpan Bool (Expr a)
              | VarPat SrcSpan a (Expr a)
  deriving (Eq, Show, Generic)

instance SrcInfo (Clause a) where
  srcSpan (VariantPat ss _ _ _ _) = ss
  srcSpan (BoolPat ss _ _)        = ss
  srcSpan (VarPat ss _ _)         = ss

-- | トップレベル宣言
data Decl a = ScDef SrcSpan a [a] (Expr a) -- ^ 環境を持たない関数（定数）宣言
            | ScAnn SrcSpan a (SType a) -- ^ 関数（定数）の型宣言
            | TypeDef SrcSpan a [a] (SType a) -- ^ 新しい型名の定義
  deriving (Eq, Show, Generic)

instance SrcInfo (Decl a) where
  srcSpan (ScDef ss _ _ _)   = ss
  srcSpan (ScAnn ss _ _)     = ss
  srcSpan (TypeDef ss _ _ _) = ss

-- | ソースコード上での型の表現
data SType a = STyApp SrcSpan (STyCon a) [SType a]
             | STyVar SrcSpan a
  deriving (Eq, Show, Generic)

instance SrcInfo (SType a) where
  srcSpan (STyApp ss _ _) = ss
  srcSpan (STyVar ss _)   = ss

{- # SType vs Type
型検査時にLanguage.Malgo.Type.Typeへ翻訳される．
Forallは型検査の過程で自動生成される．
-}

data STyCon a = SimpleC SrcSpan a
              | SRecordC SrcSpan [(Text, SType a)]
              | SVariantC SrcSpan [(Text, SType a)]
  deriving (Eq, Show, Generic)

instance SrcInfo (STyCon a) where
  srcSpan (SimpleC ss _)   = ss
  srcSpan (SRecordC ss _)  = ss
  srcSpan (SVariantC ss _) = ss

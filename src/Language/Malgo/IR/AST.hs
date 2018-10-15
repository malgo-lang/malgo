{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.IR.AST where

import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.Pretty
import           Language.Malgo.Type
import           Universum                   hiding (Type)

data Expr a = Var SrcSpan a
            | Literal SrcSpan Literal
            | BinOp SrcSpan Op (Expr a) (Expr a)
            | If SrcSpan (Expr a) (Expr a) (Expr a)
            | Let SrcSpan (Bind a) (Expr a)
            | Apply SrcSpan (Expr a) (Expr a)
            | Tuple SrcSpan [Expr a]
            | Access SrcSpan (Expr a) Int
  deriving (Eq, Show)

instance SrcInfo (Expr a)
instance Pretty a => Pretty (Expr a)

data Literal = Int Integer
             | Float Double
             | Bool Bool
             | Char Char
             | String String
  deriving (Eq, Show)

instance Pretty Literal

data Op = Add | Sub | Mul | Div | Mod
        | FAdd | FSub | FMul | FDiv
        | Eq | Neq | Lt | Gt | Le | Ge
        | And | Or
  deriving (Eq, Show)

instance Pretty Op

data Bind a = NonRec SrcSpan a (Maybe (Type a)) (Expr a)
            | Rec SrcSpan a [a] (Maybe (Type a)) (Expr a)
  deriving (Eq, Show)

instance SrcInfo (Bind a)
instance Pretty a => Pretty (Bind a)

-- | トップレベル宣言
data Decl a = ScDef SrcSpan a [a] (Expr a) -- ^ 環境を持たない関数（定数）宣言
            | ScAnn SrcSpan a (Type a) -- ^ 関数（定数）の型宣言
            | TypeDef SrcSpan a [a] (Type a) -- ^ 型の別名宣言
  deriving (Eq, Show, Generic)

instance SrcInfo (Decl a)
instance Pretty a => Pretty (Decl a)

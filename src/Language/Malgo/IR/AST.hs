{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
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

instance SrcInfo (Expr a) where
  srcSpan (Var ss _)       = ss
  srcSpan (Literal ss _)   = ss
  srcSpan (BinOp ss _ _ _) = ss
  srcSpan (If ss _ _ _)    = ss
  srcSpan (Let ss _ _)     = ss
  srcSpan (Apply ss _ _)   = ss
  srcSpan (Tuple ss _)     = ss
  srcSpan (Access ss _ _)  = ss

instance Pretty a => Pretty (Expr a) where
  pPrintPrec _ _ (Var _ x) = pPrint x
  pPrintPrec _ _ (Literal _ x) = pPrint x
  pPrintPrec l d (BinOp _ op x y) =
    maybeParens (d > 5) $ pPrintPrec l 6 x <+> pPrint op <+> pPrintPrec l 6 y
  pPrintPrec l d (If _ c t f) =
    maybeParens (d > 3)
    $ "if" <+> pPrintPrec l 4 c
    $+$ "then" <+> pPrintPrec l 4 t
    $+$ "else" <+> pPrintPrec l 4 f
  pPrintPrec l d (Let _ bind expr) =
    maybeParens (d > 1)
    $ "let" <+> pPrintPrec l 0 bind
    $+$ "in" <+> pPrintPrec l 0 expr
  pPrintPrec l d (Apply _ x y) =
    maybeParens (d > 10) $ pPrintPrec l 11 x <+> pPrintPrec l 11 y
  pPrintPrec l _ (Tuple _ xs) =
    parens $ sep $ punctuate "," $ map (pPrintPrec l 0) xs
  pPrintPrec l d (Access _ x i) =
    maybeParens (d > 10) $ pPrintPrec l 11 x <> "." <> pPrint i

data Literal = Int Integer
             | Float Double
             | Bool Bool
             | Char Char
             | String Text
  deriving (Eq, Show)

instance Pretty Literal where
  pPrint (Int i)      = pPrint i
  pPrint (Float d)    = pPrint d
  pPrint (Bool True)  = "true"
  pPrint (Bool False) = "false"
  pPrint (Char c)     = quotes $ pPrint c
  pPrint (String s)   = doubleQuotes $ pPrint s

data Op = Add | Sub | Mul | Div | Mod
        | FAdd | FSub | FMul | FDiv
        | Eq | Neq | Lt | Gt | Le | Ge
        | And | Or
  deriving (Eq, Show)

instance Pretty Op where
  pPrint op = case op of
    { Add -> "+"; Sub -> "-"; Mul -> "*"; Div -> "/"; Mod -> "%"
    ; FAdd -> "+."; FSub -> "-."; FMul -> "*."; FDiv -> "/."
    ; Eq -> "=="; Neq -> "/="; Lt -> "<"; Gt -> ">"; Le -> "<="; Ge -> ">="
    ; And -> "&"; Or -> "|" }

data Bind a = NonRec SrcSpan a (Maybe (TypeScheme a)) (Expr a)
            | Rec SrcSpan a [a] (Maybe (TypeScheme a)) (Expr a)
  deriving (Eq, Show)

instance SrcInfo (Bind a) where
  srcSpan (NonRec ss _ _ _) = ss
  srcSpan (Rec ss _ _ _ _)  = ss

instance Pretty a => Pretty (Bind a) where
  pPrint (NonRec _ x Nothing e) = pPrint x <+> "=" <+> pPrint e
  pPrint (NonRec _ x (Just t) e) = pPrint x <+> ":" <+> pPrint t <+> "=" <+> pPrint e
  pPrint (Rec _ f xs Nothing e) =
    "rec" <+> pPrint f <+> sep (map pPrint xs) <+> "="
    $+$ nest 2 (pPrint e)
  pPrint (Rec _ f xs (Just t) e) =
    "rec" <+> pPrint f <+> sep (map pPrint xs) <+> ":" <+> pPrint t <+> "="
    $+$ nest 2 (pPrint e)

-- | トップレベル宣言
data Decl a = ScDef SrcSpan a [a] (Expr a) -- ^ 環境を持たない関数（定数）宣言
            | ScAnn SrcSpan a (TypeScheme a) -- ^ 関数（定数）の型宣言
            | TypeDef SrcSpan a [a] (Type a) -- ^ 型の別名宣言
  deriving (Eq, Show, Generic)

instance SrcInfo (Decl a) where
  srcSpan (ScDef ss _ _ _)   = ss
  srcSpan (ScAnn ss _ _)     = ss
  srcSpan (TypeDef ss _ _ _) = ss

instance Pretty a => Pretty (Decl a) where
  pPrint (ScDef _ f xs e) =
    pPrint f <+> sep (map pPrint xs) <+> "="
    $+$ nest 2 (pPrint e)
  pPrint (ScAnn _ x t) = pPrint x <+> ":" <+> pPrint t
  pPrint (TypeDef _ n ps t) = "type" <+> pPrint n <+> sep (map pPrint ps) <+> "=" <+> pPrint t

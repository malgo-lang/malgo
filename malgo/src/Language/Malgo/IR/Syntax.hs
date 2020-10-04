{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Malgoの抽象構文木の定義
module Language.Malgo.IR.Syntax
  ( module Export,
    Decl (..),
    Expr (..),
    Pat (..),
    position,
  )
where

import Koriel.Prelude hiding (ix)
import Language.Malgo.IR.Op as Export
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.SType
import Language.Malgo.TypeRep.Type
import Text.Parsec.Pos (SourcePos)
import Text.PrettyPrint.HughesPJClass
  ( braces,
    brackets,
    doubleQuotes,
    parens,
    punctuate,
    quotes,
    sep,
    ($+$),
  )

-- | 式。
-- 型変数aは識別子の型を表す。ParserはExpr Stringを生成する。
data Expr a
  = Var SourcePos a
  | Int SourcePos Integer
  | Float SourcePos Double
  | Bool SourcePos Bool
  | Char SourcePos Char
  | String SourcePos Text
  | Tuple SourcePos [Expr a]
  | Array SourcePos (NonEmpty (Expr a))
  | MakeArray SourcePos (Expr a) (Expr a)
  | ArrayRead SourcePos (Expr a) (Expr a)
  | ArrayWrite SourcePos (Expr a) (Expr a) (Expr a)
  | Call SourcePos (Expr a) [Expr a]
  | Fn SourcePos [(a, Maybe (SType a))] (Expr a)
  | Seq SourcePos (Expr a) (Expr a)
  | Let SourcePos (Decl a) (Expr a)
  | If SourcePos (Expr a) (Expr a) (Expr a)
  | BinOp SourcePos Op (Expr a) (Expr a)
  | Match SourcePos (Expr a) (NonEmpty (Pat a, Expr a))
  deriving stock (Eq, Show, Functor)

-- | 変数定義、関数定義、外部関数定義
data Decl a
  = FunDec
      [ ( SourcePos,
          a,
          [(a, Maybe (SType a))],
          Maybe (SType a),
          Expr a
        )
      ]
  | ValDec SourcePos a (Maybe (SType a)) (Expr a)
  | ExDec SourcePos a (SType a) Text
  deriving stock (Eq, Show, Functor)

-- | パターン
data Pat a
  = VarP SourcePos a
  | TupleP SourcePos [Pat a]
  deriving stock (Eq, Show, Functor)

-- | Exprからソースコード上の位置情報を取り出すための補助関数
position :: Expr t -> SourcePos
position (Var i _) = i
position (Int i _) = i
position (Float i _) = i
position (Bool i _) = i
position (Char i _) = i
position (String i _) = i
position (Tuple i _) = i
position (Array i _) = i
position (MakeArray i _ _) = i
position (ArrayRead i _ _) = i
position (ArrayWrite i _ _ _) = i
position (Call i _ _) = i
position (Fn i _ _) = i
position (Seq i _ _) = i
position (Let i _ _) = i
position (If i _ _ _) = i
position (BinOp i _ _ _) = i
position (Match i _ _) = i

instance Pretty a => Pretty (Expr a) where
  pPrint (Var _ name) = pPrint name
  pPrint (Int _ x) = pPrint x
  pPrint (Float _ x) = pPrint x
  pPrint (Bool _ True) = "true"
  pPrint (Bool _ False) = "false"
  pPrint (Char _ x) = quotes $ pPrint x
  pPrint (String _ x) = doubleQuotes $ pPrint x
  pPrint (Tuple _ xs) = braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (Array _ xs) = brackets $ sep $ punctuate "," $ toList $ fmap pPrint xs
  pPrint (MakeArray _ x n) = parens $ "array" <+> pPrint x <+> pPrint n
  pPrint (ArrayRead _ arr ix) = pPrint arr <> brackets (pPrint ix)
  pPrint (ArrayWrite _ arr ix val) =
    parens $ "<-" <+> (pPrint arr <> brackets (pPrint ix)) <+> pPrint val
  pPrint (Call _ fn arg) = parens $ pPrint fn <+> sep (map pPrint arg)
  pPrint (Fn _ params body) =
    parens $ "fn" <+> parens (sep $ punctuate "," (map (pPrint . fst) params)) <+> pPrint body
  pPrint (Seq _ e1 e2) = parens $ "seq" <+> (pPrint e1 $+$ pPrint e2)
  pPrint (Let _ decl body) = parens $ "let" <+> pPrint decl $+$ pPrint body
  pPrint (If _ c t f) = parens $ "if" <+> pPrint c $+$ pPrint t $+$ pPrint f
  pPrint (BinOp _ op x y) = parens $ sep [pPrint op, pPrint x, pPrint y]
  pPrint (Match _ s cs) =
    parens $
      "match" <+> pPrint s
        $+$ sep
          (punctuate "|" (toList $ fmap pPrintClause cs))
    where
      pPrintClause (p, e) = pPrint p <+> "=>" <+> pPrint e

instance HasType a => HasType (Expr a) where
  typeOf (Var _ x) = typeOf x
  typeOf (Int _ _) = intTy
  typeOf (Float _ _) = floatTy
  typeOf (Bool _ _) = boolTy
  typeOf (Char _ _) = charTy
  typeOf (String _ _) = stringTy
  typeOf (Tuple _ xs) = tupleTy $ map typeOf xs
  typeOf (Array _ (x :| _)) = arrayTy $ typeOf x
  typeOf (MakeArray _ x _) = arrayTy $ typeOf x
  typeOf (ArrayRead _ (typeOf -> TyApp ArrayC [t]) _) = t
  typeOf ArrayRead {} = bug Unreachable
  typeOf ArrayWrite {} = tupleTy []
  typeOf (Call _ (typeOf -> _ :-> t) _) = t
  typeOf Call {} = bug Unreachable
  typeOf (Fn _ _ e) = typeOf e
  typeOf (Seq _ _ e) = typeOf e
  typeOf (Let _ _ e) = typeOf e
  typeOf (If _ _ e _) = typeOf e
  typeOf (BinOp _ Add _ _) = intTy
  typeOf (BinOp _ Sub _ _) = intTy
  typeOf (BinOp _ Mul _ _) = intTy
  typeOf (BinOp _ Div _ _) = intTy
  typeOf (BinOp _ Mod _ _) = intTy
  typeOf (BinOp _ FAdd _ _) = floatTy
  typeOf (BinOp _ FSub _ _) = floatTy
  typeOf (BinOp _ FMul _ _) = floatTy
  typeOf (BinOp _ FDiv _ _) = floatTy
  typeOf (BinOp _ Eq _ _) = boolTy
  typeOf (BinOp _ Neq _ _) = boolTy
  typeOf (BinOp _ Lt _ _) = boolTy
  typeOf (BinOp _ Gt _ _) = boolTy
  typeOf (BinOp _ Le _ _) = boolTy
  typeOf (BinOp _ Ge _ _) = boolTy
  typeOf (BinOp _ And _ _) = boolTy
  typeOf (BinOp _ Or _ _) = boolTy
  typeOf (Match _ _ ((_, e) :| _)) = typeOf e

instance Pretty a => Pretty (Pat a) where
  pPrint (VarP _ x) = pPrint x
  pPrint (TupleP _ xs) = braces $ sep $ punctuate "," $ map pPrint xs

instance HasType a => HasType (Pat a) where
  typeOf (VarP _ x) = typeOf x
  typeOf (TupleP _ xs) = TyApp TupleC $ map typeOf xs

instance Pretty a => Pretty (Decl a) where
  pPrint (FunDec fs) = sep $ map pp fs
    where
      pp (_, name, params, ret, body) =
        parens $
          "fun"
            <+> ( parens
                    . sep
                    $ pPrint name :
                    map (\(n, t) -> pPrint n <> maybe mempty ((":" <>) . pPrint) t) params
                )
            <> maybe mempty ((":" <>) . pPrint) ret
            $+$ pPrint body
  pPrint (ValDec _ name t val) =
    parens $ "val" <+> pPrint name <> maybe mempty ((":" <>) . pPrint) t <+> pPrint val
  pPrint (ExDec _ name t orig) =
    parens $ "extern" <+> pPrint name <> ":" <> pPrint t <+> pPrint orig

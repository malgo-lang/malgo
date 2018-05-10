{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Language.Malgo.Syntax where

import           Language.Malgo.Prelude
import           Language.Malgo.Type
import           Text.PrettyPrint

data Expr a
  -- | 変数参照
  = Var Info a
  -- | 32bit整数
  | Int Info Integer
  -- | 倍精度浮動小数点数
  | Float Info Double
  -- | 真(#t) 偽(#f)
  | Bool Info Bool
  -- | シングルクォートで囲まれた一文字
  | Char Info Char
  -- | ダブルクォートで囲まれた文字列
  | String Info Text
  -- | 空の値("()")
  | Unit Info
  -- | タプル
  | Tuple Info [Expr a]
  | TupleAccess Info (Expr a) Int
  -- | 関数呼び出し
  | Call Info (Expr a) [Expr a]
  -- | 無名関数
  | Fn Info [(a, Type)] (Expr a)
  -- | 連続した式(e1 ; e2)
  | Seq Info (Expr a) (Expr a)
  -- | let式
  | Let Info [Decl a] (Expr a)
  -- | if式
  | If Info (Expr a) (Expr a) (Expr a)
  -- | 中置演算子
  | BinOp Info Op (Expr a) (Expr a)
  deriving (Eq, Show, Read)

info :: Expr t -> Info
info (Var i _)           = i
info (Int i _)           = i
info (Float i _)         = i
info (Bool i _)          = i
info (Char i _)          = i
info (String i _)        = i
info (Tuple i _)         = i
info (TupleAccess i _ _) = i
info (Unit i)            = i
info (Call i _ _)        = i
info (Fn i _ _)          = i
info (Seq i _ _)         = i
info (Let i _ _)         = i
info (If i _ _ _)        = i
info (BinOp i _ _ _)     = i

instance Outputable a => Outputable (Expr a) where
  ppr (Var _ name) = ppr name
  ppr (Int _ x) = integer x
  ppr (Float _ x) = double x
  ppr (Bool _ True) = text "#t"
  ppr (Bool _ False) = text "#f"
  ppr (Char _ x) = quotes $ char x
  ppr (String _ x) = doubleQuotes $ ppr x
  ppr (Tuple _ xs) = braces $ ppr xs
  ppr (TupleAccess _ e i) = parens (text "." <+> ppr e <+> int i)
  ppr (Unit _) = text "{}"
  ppr (Call _ fn arg) = parens $ ppr fn <+> sep (map ppr arg)
  ppr (Fn _ params body) =
    parens $ text "fn" <+> parens (ppr $ map fst params) <+> ppr body
  ppr (Seq _ e1 e2) = parens $ text "seq" $+$ ppr e1 $+$ ppr e2
  ppr (Let _ decls body) =
    parens $
    text "let" <+> (parens . sep $ map ppr decls) $+$ nest 2 (ppr body)
  ppr (If _ c t f) =
    parens $ text "if" <+> ppr c $+$ nest 2 (ppr t) $+$ nest 2 (ppr f)
  ppr (BinOp _ op x y) = parens $ sep [ppr op, ppr x, ppr y]

-- | 中置演算子の種類を表すタグ
data Op
  = Add
  | Sub
  | Mul
  | Div
  | FAdd
  | FSub
  | FMul
  | FDiv
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
  deriving (Eq, Show, Read)

instance Outputable Op where
  ppr Add  = text "+"
  ppr Sub  = text "-"
  ppr Mul  = text "*"
  ppr Div  = text "/"
  ppr FAdd = text "+."
  ppr FSub = text "-."
  ppr FMul = text "*."
  ppr FDiv = text "/."
  ppr Mod  = text "%"
  ppr Eq   = text "=="
  ppr Neq  = text "<>"
  ppr Lt   = text "<"
  ppr Gt   = text ">"
  ppr Le   = text "<="
  ppr Ge   = text ">="
  ppr And  = text "&&"
  ppr Or   = text "||"

data Decl a
  = FunDec Info a [(a, Type)] Type (Expr a)
  | ValDec Info a (Maybe Type) (Expr a)
  | ExDec Info a Type Text
  deriving (Eq, Show, Read)

instance Outputable a => Outputable (Decl a) where
  ppr (FunDec _ name params _ body) =
    parens $
    text "fun" <+>
    (parens . sep $ ppr name : map (\(n, _) -> ppr n) params) $+$
    nest 2 (ppr body)
  ppr (ValDec _ name _ val) =
    parens $ text "val" <+> ppr name <+> ppr val
  ppr (ExDec _ name _ orig) =
    parens $ text "extern" <+> ppr name <+> ppr orig

instance Typeable a => Typeable (Expr a) where
    typeOf (Var _ name) = typeOf name
    typeOf (Int _ _) = "Int"
    typeOf (Float _ _) = "Float"
    typeOf (Bool _ _) = "Bool"
    typeOf (Char _ _) = "Char"
    typeOf (String _ _) = "String"
    typeOf (Tuple _ xs) = TupleTy (map typeOf xs)
    typeOf (TupleAccess _ e i) =
      let TupleTy xs = typeOf e
      in fromMaybe (panic "out of bounds") (atMay xs i)
    typeOf (Unit _) = "Unit"
    typeOf (Fn _ params body) = FunTy (map snd params) (typeOf body)
    typeOf (Call _ fn _) =
        case typeOf fn of
            (FunTy _ ty) -> ty
            _            -> panic "(typeOf fn) should match (FunTy _ ty)"
    typeOf (Seq _ _ e) = typeOf e
    typeOf (Let _ _ e) = typeOf e
    typeOf (If _ _ e _) = typeOf e
    typeOf (BinOp i op x _) =
        case typeOfOp i op (typeOf x) of
            (FunTy _ ty) -> ty
            _            -> panic "(typeOfOp op) should match (FunTy _ ty)"

typeOfOp :: Info -> Op -> Type -> Type
typeOfOp _ Add _ = FunTy ["Int", "Int"] "Int"
typeOfOp _ Sub _ = FunTy ["Int", "Int"] "Int"
typeOfOp _ Mul _ = FunTy ["Int", "Int"] "Int"
typeOfOp _ Div _ = FunTy ["Int", "Int"] "Int"
typeOfOp _ FAdd _ = FunTy ["Float", "Float"] "Float"
typeOfOp _ FSub _ = FunTy ["Float", "Float"] "Float"
typeOfOp _ FMul _ = FunTy ["Float", "Float"] "Float"
typeOfOp _ FDiv _ = FunTy ["Float", "Float"] "Float"
typeOfOp _ Mod _ = FunTy ["Int", "Int"] "Int"
typeOfOp i Eq ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else panic $ show (ppr i <+> ":" <+> ppr ty <+> "is not comparable")
typeOfOp i Neq ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else panic $ show (ppr i <+> ":" <+> ppr ty <+> "is not comparable")
typeOfOp i Lt ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else panic $ show (ppr i <+> ":" <+> ppr ty <+> "is not comparable")
typeOfOp i Gt ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else panic $ show (ppr i <+> ":" <+> ppr ty <+> "is not comparable")
typeOfOp i Le ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else panic $ show (ppr i <+> ":" <+> ppr ty <+> "is not comparable")
typeOfOp i Ge ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else panic $ show (ppr i <+> ":" <+> ppr ty <+> "is not comparable")
typeOfOp _ And _ = FunTy ["Bool", "Bool"] "Bool"
typeOfOp _ Or _ = FunTy ["Bool", "Bool"] "Bool"

comparable :: Type -> Bool
comparable "Int"      = True
comparable "Float"    = True
comparable "Bool"     = True
comparable "Char"     = True
comparable "String"   = False -- TODO: Stringの比較をサポート
comparable "Unit"     = False
comparable NameTy {}  = False
comparable FunTy {}   = False
comparable ClsTy {}   = False
comparable TupleTy {} = False

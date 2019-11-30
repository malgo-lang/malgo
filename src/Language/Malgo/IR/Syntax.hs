{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.IR.Syntax where

import           Data.List                    ((!!))
import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Relude                       hiding (Op, Type)

data Expr a
  -- | 変数参照
  = Var Info a
  -- | 64bit整数
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
  -- | 配列作成
  | MakeArray Info Type (Expr a)
  | ArrayRead Info
    (Expr a) -- array
    (Expr a) -- index
  | ArrayWrite Info
    (Expr a) -- array
    (Expr a) -- index
    (Expr a) -- value
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
  deriving (Eq, Show, Read, Generic, PrettyVal)

info :: Expr t -> Info
info (Var i _)            = i
info (Int i _)            = i
info (Float i _)          = i
info (Bool i _)           = i
info (Char i _)           = i
info (String i _)         = i
info (Tuple i _)          = i
info (TupleAccess i _ _)  = i
info (MakeArray i _ _)    = i
info (ArrayRead i _ _)    = i
info (ArrayWrite i _ _ _) = i
info (Unit i)             = i
info (Call i _ _)         = i
info (Fn i _ _)           = i
info (Seq i _ _)          = i
info (Let i _ _)          = i
info (If i _ _ _)         = i
info (BinOp i _ _ _)      = i

instance Pretty a => Pretty (Expr a) where
  pPrint (Var _ name) = pPrint name
  pPrint (Int _ x) = pPrint x
  pPrint (Float _ x) = pPrint x
  pPrint (Bool _ True) = "true"
  pPrint (Bool _ False) = "false"
  pPrint (Char _ x) = quotes $ pPrint x
  pPrint (String _ x) = doubleQuotes $ pPrint x
  pPrint (Tuple _ xs) =
    braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (TupleAccess _ e i) =
    parens ("." <+> pPrint e <+> pPrint i)
  pPrint (MakeArray _ ty size) =
    parens $ "array" <+> pPrint ty <+> pPrint size
  pPrint (ArrayRead _ arr ix) =
    pPrint arr <> brackets (pPrint ix)
  pPrint (ArrayWrite _ arr ix val) =
    parens $ "<-" <+> (pPrint arr <> brackets (pPrint ix)) <+> pPrint val
  pPrint (Unit _) = "{}"
  pPrint (Call _ fn arg) =
    parens $ pPrint fn <+> sep (map pPrint arg)
  pPrint (Fn _ params body) =
    parens $ "fn"
    <+> parens (sep $ punctuate "," (map (pPrint . fst) params))
    <+> pPrint body
  pPrint (Seq _ e1 e2) =
    parens $ "seq" <+> (pPrint e1 $+$ pPrint e2)
  pPrint (Let _ decls body) =
    parens $
    "let" <+> (parens . sep $ map pPrint decls)
    $+$ nest 2 (pPrint body)
  pPrint (If _ c t f) =
    parens $ "if" <+> pPrint c
    $+$ nest 2 (pPrint t)
    $+$ nest 2 (pPrint f)
  pPrint (BinOp _ op x y) =
    parens $ sep [pPrint op, pPrint x, pPrint y]

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
  deriving (Eq, Show, Read, Generic, PrettyVal)

instance Pretty Op where
  pPrint Add  = "+"
  pPrint Sub  = "-"
  pPrint Mul  = "*"
  pPrint Div  = "/"
  pPrint FAdd = "+."
  pPrint FSub = "-."
  pPrint FMul = "*."
  pPrint FDiv = "/."
  pPrint Mod  = "%"
  pPrint Eq   = "=="
  pPrint Neq  = "<>"
  pPrint Lt   = "<"
  pPrint Gt   = ">"
  pPrint Le   = "<="
  pPrint Ge   = ">="
  pPrint And  = "&&"
  pPrint Or   = "||"

data Decl a
  = FunDec Info a [(a, Type)] Type (Expr a)
  | ValDec Info a (Maybe Type) (Expr a)
  | ExDec Info a Type Text
  deriving (Eq, Show, Read, Generic, PrettyVal)

instance Pretty a => Pretty (Decl a) where
  pPrint (FunDec _ name params _ body) =
    parens $ "fun" <+>
    (parens . sep $ pPrint name : map (\(n, _) -> pPrint n) params)
    $+$ nest 2 (pPrint body)
  pPrint (ValDec _ name _ val) =
    parens $ "val" <+> pPrint name <+> pPrint val
  pPrint (ExDec _ name _ orig) =
    parens $ "extern" <+> pPrint name <+> pPrint orig

instance HasType a => HasType (Expr a) where
    typeOf (Var _ name) = typeOf name
    typeOf (Int _ _) = TyInt
    typeOf (Float _ _) = TyFloat
    typeOf (Bool _ _) = TyBool
    typeOf (Char _ _) = TyChar
    typeOf (String _ _) = TyString
    typeOf (Tuple _ xs) = TyTuple (map typeOf xs)
    typeOf (TupleAccess _ e i) =
      case typeOf e of
        TyTuple xs -> xs !! i
        _          -> error "(typeOf e) should match (TyTuple xs)"
    typeOf (MakeArray _ t _) = TyArray t
    typeOf (ArrayRead _ arr _) =
      case typeOf arr of
        TyArray t -> t
        _         -> error "(typeof arr) should match (TyArray xs)"
    typeOf ArrayWrite{} = TyTuple []
    typeOf (Unit _) = TyTuple []
    typeOf (Fn _ params body) = TyFun (map snd params) (typeOf body)
    typeOf (Call _ fn _) =
        case typeOf fn of
            (TyFun _ ty) -> ty
            _            -> error "(typeOf fn) should match (TyFun _ ty)"
    typeOf (Seq _ _ e) = typeOf e
    typeOf (Let _ _ e) = typeOf e
    typeOf (If _ _ e _) = typeOf e
    typeOf (BinOp i op x _) =
        case typeOfOp i op (typeOf x) of
            (_, _, ty) -> ty

typeOfOp :: Info -> Op -> Type -> (Type, Type, Type)
typeOfOp _ Add _ = (TyInt, TyInt, TyInt)
typeOfOp _ Sub _ = (TyInt, TyInt, TyInt)
typeOfOp _ Mul _ = (TyInt, TyInt, TyInt)
typeOfOp _ Div _ = (TyInt, TyInt, TyInt)
typeOfOp _ FAdd _ = (TyFloat, TyFloat, TyFloat)
typeOfOp _ FSub _ = (TyFloat, TyFloat, TyFloat)
typeOfOp _ FMul _ = (TyFloat, TyFloat, TyFloat)
typeOfOp _ FDiv _ = (TyFloat, TyFloat, TyFloat)
typeOfOp _ Mod _ = (TyInt, TyInt, TyInt)
typeOfOp i Eq ty =
    if comparable ty
        then (ty, ty, TyBool)
        else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Neq ty =
    if comparable ty
        then (ty, ty, TyBool)
        else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Lt ty =
    if comparable ty
        then (ty, ty, TyBool)
        else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Gt ty =
    if comparable ty
        then (ty, ty, TyBool)
        else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Le ty =
    if comparable ty
        then (ty, ty, TyBool)
        else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Ge ty =
    if comparable ty
        then (ty, ty, TyBool)
        else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp _ And _ = (TyBool, TyBool, TyBool)
typeOfOp _ Or _ = (TyBool, TyBool, TyBool)


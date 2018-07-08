{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Language.Malgo.IR.Syntax where

import           Data.Text.Prettyprint.Doc
import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.Type
import           RIO                          hiding (Typeable)
import           RIO.List.Partial

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

instance Pretty a => Pretty (Expr a) where
  pretty (Var _ name) = pretty name
  pretty (Int _ x) = pretty x
  pretty (Float _ x) = pretty x
  pretty (Bool _ True) = "#t"
  pretty (Bool _ False) = "#f"
  pretty (Char _ x) = squotes $ pretty x
  pretty (String _ x) = dquotes $ pretty x
  pretty (Tuple _ xs) =
    braces $ sep $ punctuate "," $ map pretty xs
  pretty (TupleAccess _ e i) =
    parens ("." <+> pretty e <+> pretty i)
  pretty (Unit _) = "{}"
  pretty (Call _ fn arg) =
    parens $ pretty fn <+> sep (map pretty arg)
  pretty (Fn _ params body) =
    parens $ "fn"
    <+> tupled (map (pretty . fst) params)
    <+> pretty body
  pretty (Seq _ e1 e2) =
    parens $ "seq" <+> (pretty e1 <> line <> pretty e2)
  pretty (Let _ decls body) =
    parens $
    "let" <+> (parens . sep $ map pretty decls)
    <> line <> indent 2 (pretty body)
  pretty (If _ c t f) =
    parens $ "if" <+> pretty c
    <> line <> indent 2 (pretty t)
    <> line <> indent 2 (pretty f)
  pretty (BinOp _ op x y) =
    parens $ sep [pretty op, pretty x, pretty y]

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

instance Pretty Op where
  pretty Add  = "+"
  pretty Sub  = "-"
  pretty Mul  = "*"
  pretty Div  = "/"
  pretty FAdd = "+."
  pretty FSub = "-."
  pretty FMul = "*."
  pretty FDiv = "/."
  pretty Mod  = "%"
  pretty Eq   = "=="
  pretty Neq  = "<>"
  pretty Lt   = "<"
  pretty Gt   = ">"
  pretty Le   = "<="
  pretty Ge   = ">="
  pretty And  = "&&"
  pretty Or   = "||"

data Decl a
  = FunDec Info a [(a, Type)] Type (Expr a)
  | ValDec Info a (Maybe Type) (Expr a)
  | ExDec Info a Type Text
  deriving (Eq, Show, Read)

instance Pretty a => Pretty (Decl a) where
  pretty (FunDec _ name params _ body) =
    parens $ "fun" <+>
    (parens . sep $ pretty name : map (\(n, _) -> pretty n) params) <> line <>
    indent 2 (pretty body)
  pretty (ValDec _ name _ val) =
    parens $ "val" <+> pretty name <+> pretty val
  pretty (ExDec _ name _ orig) =
    parens $ "extern" <+> pretty name <+> pretty orig

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
      in xs !! i
    typeOf (Unit _) = "Unit"
    typeOf (Fn _ params body) = FunTy (map snd params) (typeOf body)
    typeOf (Call _ fn _) =
        case typeOf fn of
            (FunTy _ ty) -> ty
            _            -> error "(typeOf fn) should match (FunTy _ ty)"
    typeOf (Seq _ _ e) = typeOf e
    typeOf (Let _ _ e) = typeOf e
    typeOf (If _ _ e _) = typeOf e
    typeOf (BinOp i op x _) =
        case typeOfOp i op (typeOf x) of
            (FunTy _ ty) -> ty
            _            -> error "(typeOfOp op) should match (FunTy _ ty)"

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
        else error $ show (pretty i <+> ":" <+> pretty ty <+> "is not comparable")
typeOfOp i Neq ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else error $ show (pretty i <+> ":" <+> pretty ty <+> "is not comparable")
typeOfOp i Lt ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else error $ show (pretty i <+> ":" <+> pretty ty <+> "is not comparable")
typeOfOp i Gt ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else error $ show (pretty i <+> ":" <+> pretty ty <+> "is not comparable")
typeOfOp i Le ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else error $ show (pretty i <+> ":" <+> pretty ty <+> "is not comparable")
typeOfOp i Ge ty =
    if comparable ty
        then FunTy [ty, ty] "Bool"
        else error $ show (pretty i <+> ":" <+> pretty ty <+> "is not comparable")
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
comparable TupleTy {} = False

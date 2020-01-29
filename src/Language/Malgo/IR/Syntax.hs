{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.IR.Syntax where

import           Data.List                      ( (!!) )
import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Language.Malgo.Prelude
import           Control.Lens                   ( view
                                                , _3
                                                )
import           Text.PrettyPrint.HughesPJClass ( quotes
                                                , doubleQuotes
                                                , braces
                                                , sep
                                                , punctuate
                                                , parens
                                                , brackets
                                                , ($+$)
                                                )

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
  | String Info String
  -- | 空の値("()")
  | Unit Info
  -- | タプル
  | Tuple Info [Expr a]
  | TupleAccess Info (Expr a) Int
  -- | 配列作成
  | MakeArray Info (Expr a) -- ^ init value
                   (Expr a) -- ^ size
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
  | Fn Info [(a, Maybe Type)] (Expr a)
  -- | 連続した式(e1 ; e2)
  | Seq Info (Expr a) (Expr a)
  -- | let式
  | Let Info (Decl a) (Expr a)
  -- | if式
  | If Info (Expr a) (Expr a) (Expr a)
  -- | 中置演算子
  | BinOp Info Op (Expr a) (Expr a)
  -- | パターンマッチ
  | Match Info (Expr a) (NonEmpty (Pat a, Expr a))
  deriving (Eq, Show, Read, Functor, Foldable, Traversable, Generic)

info :: Expr t -> Info
info (Var    i _        ) = i
info (Int    i _        ) = i
info (Float  i _        ) = i
info (Bool   i _        ) = i
info (Char   i _        ) = i
info (String i _        ) = i
info (Tuple  i _        ) = i
info (TupleAccess i _ _ ) = i
info (MakeArray   i _ _ ) = i
info (ArrayRead   i _ _ ) = i
info (ArrayWrite i _ _ _) = i
info (Unit i            ) = i
info (Call i _ _        ) = i
info (Fn   i _ _        ) = i
info (Seq  i _ _        ) = i
info (Let  i _ _        ) = i
info (If    i _ _ _     ) = i
info (BinOp i _ _ _     ) = i
info (Match i _ _       ) = i

instance Pretty a => Pretty (Expr a) where
  pPrint (Var    _ name          ) = pPrint name
  pPrint (Int    _ x             ) = pPrint x
  pPrint (Float  _ x             ) = pPrint x
  pPrint (Bool   _ True          ) = "true"
  pPrint (Bool   _ False         ) = "false"
  pPrint (Char   _ x             ) = quotes $ pPrint x
  pPrint (String _ x             ) = doubleQuotes $ pPrint x
  pPrint (Tuple  _ xs            ) = braces $ sep $ punctuate "," $ map pPrint xs
  pPrint (TupleAccess _ e    i   ) = parens ("." <+> pPrint e <+> pPrint i)
  pPrint (MakeArray   _ init size) = parens $ "array" <+> pPrint init <+> pPrint size
  pPrint (ArrayRead   _ arr  ix  ) = pPrint arr <> brackets (pPrint ix)
  pPrint (ArrayWrite _ arr ix val) =
    parens $ "<-" <+> (pPrint arr <> brackets (pPrint ix)) <+> pPrint val
  pPrint (Unit _       ) = "{}"
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

data Pat a = VarP a
           | TupleP [Pat a]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable, Generic)

instance Pretty a => Pretty (Pat a) where
  pPrint (VarP   x ) = pPrint x
  pPrint (TupleP xs) = braces $ sep $ punctuate "," $ map pPrint xs

instance HasType a => HasType (Pat a) where
  typeOf (VarP   x ) = typeOf x
  typeOf (TupleP xs) = TyApp TupleC $ map typeOf xs

-- | 中置演算子の種類を表すタグ
data Op = Add | Sub | Mul | Div
        | FAdd | FSub | FMul | FDiv
        | Mod
        | Eq | Neq
        | Lt | Gt | Le | Ge
        | And | Or
  deriving (Eq, Show, Read, Generic)

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
  = FunDec [(Info, a, [(a, Maybe Type)], Maybe Type, Expr a)]
  | ValDec Info a (Maybe Type) (Expr a)
  | ExDec Info a Type String
  deriving (Eq, Show, Read, Functor, Foldable, Traversable, Generic)

instance Pretty a => Pretty (Decl a) where
  pPrint (FunDec fs) = sep $ map pp fs
   where
    pp (_, name, params, _, body) =
      parens
        $   "fun"
        <+> (parens . sep $ pPrint name : map (\(n, _) -> pPrint n) params)
        $+$ pPrint body
  pPrint (ValDec _ name _ val ) = parens $ "val" <+> pPrint name <+> pPrint val
  pPrint (ExDec  _ name _ orig) = parens $ "extern" <+> pPrint name <+> pPrint orig

instance HasType a => HasType (Expr a) where
  typeOf (Var    _ name    ) = typeOf name
  typeOf (Int    _ _       ) = TyApp IntC []
  typeOf (Float  _ _       ) = TyApp FloatC []
  typeOf (Bool   _ _       ) = TyApp BoolC []
  typeOf (Char   _ _       ) = TyApp CharC []
  typeOf (String _ _       ) = TyApp StringC []
  typeOf (Tuple  _ xs      ) = TyApp TupleC (map typeOf xs)
  typeOf (TupleAccess _ e i) = case typeOf e of
    TyApp TupleC xs -> xs !! i
    _               -> error "(typeOf e) should match (TyTuple xs)"
  typeOf (MakeArray _ init _) = TyApp ArrayC [typeOf init]
  typeOf (ArrayRead _ arr  _) = case typeOf arr of
    TyApp ArrayC [t] -> t
    _                -> error "(typeof arr) should match (TyArray xs)"
  typeOf ArrayWrite{}         = TyApp TupleC []
  typeOf (Unit _            ) = TyApp TupleC []
  typeOf (Fn   _ params body) = TyApp FunC $ typeOf body : map (typeOf . fst) params
  typeOf (Call _ fn     _   ) = case typeOf fn of
    (TyApp FunC (ret : _)) -> ret
    _                      -> error "(typeOf fn) should match (TyFun _ ty)"
  typeOf (Seq _ _ e              ) = typeOf e
  typeOf (Let _ _ e              ) = typeOf e
  typeOf (If    _ _  e _         ) = typeOf e
  typeOf (BinOp i op x _         ) = view _3 $ typeOfOp i op (typeOf x)
  typeOf (Match _ _ ((_, e) :| _)) = typeOf e

typeOfOp :: Info -> Op -> Type -> (Type, Type, Type)
typeOfOp _ Add  _  = (TyApp IntC [], TyApp IntC [], TyApp IntC [])
typeOfOp _ Sub  _  = (TyApp IntC [], TyApp IntC [], TyApp IntC [])
typeOfOp _ Mul  _  = (TyApp IntC [], TyApp IntC [], TyApp IntC [])
typeOfOp _ Div  _  = (TyApp IntC [], TyApp IntC [], TyApp IntC [])
typeOfOp _ FAdd _  = (TyApp FloatC [], TyApp FloatC [], TyApp FloatC [])
typeOfOp _ FSub _  = (TyApp FloatC [], TyApp FloatC [], TyApp FloatC [])
typeOfOp _ FMul _  = (TyApp FloatC [], TyApp FloatC [], TyApp FloatC [])
typeOfOp _ FDiv _  = (TyApp FloatC [], TyApp FloatC [], TyApp FloatC [])
typeOfOp _ Mod  _  = (TyApp IntC [], TyApp IntC [], TyApp IntC [])
typeOfOp i Eq   ty = if comparable ty
  then (ty, ty, TyApp BoolC [])
  else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Neq ty = if comparable ty
  then (ty, ty, TyApp BoolC [])
  else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Lt ty = if comparable ty
  then (ty, ty, TyApp BoolC [])
  else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Gt ty = if comparable ty
  then (ty, ty, TyApp BoolC [])
  else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Le ty = if comparable ty
  then (ty, ty, TyApp BoolC [])
  else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp i Ge ty = if comparable ty
  then (ty, ty, TyApp BoolC [])
  else error $ show (pPrint i <+> ":" <+> pPrint ty <+> "is not comparable")
typeOfOp _ And _ = (TyApp BoolC [], TyApp BoolC [], TyApp BoolC [])
typeOfOp _ Or  _ = (TyApp BoolC [], TyApp BoolC [], TyApp BoolC [])


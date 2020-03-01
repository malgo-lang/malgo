{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Malgo.IR.LIR where

import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.TypeRep.LType

import           Text.PrettyPrint.HughesPJClass ( ($$)
                                                , vcat
                                                , parens
                                                , sep
                                                , punctuate
                                                , ($+$)
                                                , nest
                                                , brackets
                                                , text
                                                )

data Program a = Program { functions :: [Func a], mainFunc :: Block a }
  deriving stock (Eq, Show, Functor, Foldable)

instance (HasLType a, Pretty a) => Pretty (Program a) where
  pPrint Program { functions, mainFunc } =
    "program" <+> pPrint mainFunc $$ vcat (map pPrint functions)

data Func a = Func { name :: a, params :: [a], body :: Block a }
  deriving stock (Eq, Show, Functor, Foldable)

instance (HasLType a, Pretty a) => Pretty (Func a) where
  pPrint Func { name, params, body } =
    "func"
      <+> pPrint name
      <>  "<"
      <>  pPrint (ltypeOf name)
      <>  ">"
      <>  parens (sep $ punctuate "," $ map pPrint params)
      $+$ nest 1 (pPrint body)

instance HasLType a => HasLType (Func a) where
  ltypeOf Func { name } = ltypeOf name

data Block a = Block { insns :: [Insn a], value :: a }
  deriving stock (Eq, Show, Functor, Foldable)

instance (HasLType a, Pretty a) => Pretty (Block a) where
  pPrint Block { insns, value } =
    brackets $ vcat (punctuate ";" $ map pPrint insns) $$ pPrint value

instance HasLType a => HasLType (Block a) where
  ltypeOf Block { value = x } = ltypeOf x

data Insn a = Assign a (Expr a)
            | StoreC a [Int] a
            | Store a [a] a
            | For a -- ^ index
                  a -- ^ from
                  a -- ^ to
                  (Block a) -- ^ body
  deriving stock (Eq, Show, Functor, Foldable)

instance (HasLType a, Pretty a) => Pretty (Insn a) where
  pPrint (Assign x e   ) = pPrint x <> ":" <> pPrint (ltypeOf x) <+> "=" <+> pPrint e
  pPrint (StoreC x is v) = "storec" <+> pPrint x <+> pPrint is <+> pPrint v
  pPrint (Store  x is v) = "store" <+> pPrint x <+> pPrint is <+> pPrint v
  pPrint (For index from to body) =
    "for"
      <+> pPrint index
      <+> "from"
      <+> pPrint from
      <+> "to"
      <+> pPrint to
      $$  "loop:"
      <+> pPrint body

data Expr a = Constant Constant
            | Call a [a]
            | CallExt String LType [a]
            | ArrayCreate LType -- ^ type of element
                          a -- ^ size
            | Alloca LType
            | LoadC a [Int]
            | Load LType a [a]
            | Cast LType a
            | Trunc LType a
            | Zext LType a
            | Sext LType a
            | Undef LType
            | BinOp Op a a
            | If a (Block a) (Block a)
  deriving stock (Eq, Show, Functor, Foldable)

instance (HasLType a, Pretty a) => Pretty (Expr a) where
  pPrint (Constant c) = pPrint c
  pPrint (Call f xs ) = pPrint f <> parens (sep $ punctuate "," $ map pPrint xs)
  pPrint (CallExt f t xs) =
    pPrint f <> "<" <> pPrint t <> ">" <> parens (sep $ punctuate "," $ map pPrint xs)
  pPrint (ArrayCreate init size) = "arrayCreate" <+> pPrint init <+> pPrint size
  pPrint (Alloca t             ) = "alloca" <+> pPrint t
  pPrint (LoadC x is           ) = "loadc" <+> pPrint x <+> pPrint is
  pPrint (Load t x i           ) = "load" <+> pPrint t <+> pPrint x <+> pPrint i
  pPrint (Cast  t a            ) = "cast" <+> pPrint t <+> pPrint a
  pPrint (Trunc t a            ) = "trunc" <+> pPrint t <+> pPrint a
  pPrint (Zext  t a            ) = "zext" <+> pPrint t <+> pPrint a
  pPrint (Sext  t a            ) = "sext" <+> pPrint t <+> pPrint a
  pPrint (Undef t              ) = "undef" <+> pPrint t
  pPrint (BinOp op x y         ) = pPrint op <+> pPrint x <+> pPrint y
  pPrint (If c t f) = "if" <+> pPrint c $$ ("then:" <+> pPrint t) $$ ("else:" <+> pPrint f)

instance (HasLType a, Show a) => HasLType (Expr a) where
  ltypeOf (Constant x) = ltypeOf x
  ltypeOf (Call (ltypeOf -> Function t _) _) = t
  ltypeOf (CallExt _ (ltypeOf -> Function t _) _) = t
  ltypeOf (ArrayCreate init _) = Ptr $ Struct [Ptr (ltypeOf init), SizeT]
  ltypeOf (Alloca t) = Ptr t
  ltypeOf (LoadC x is) = accessType (ltypeOf x) is
  ltypeOf (Load t _ _) = t
  ltypeOf (Cast t _) = t
  ltypeOf (Undef t) = t
  ltypeOf (BinOp op x _) = ltypeOfOp op (ltypeOf x)
  ltypeOf (If _ x _) = ltypeOf x
  ltypeOf t = error $ toText $ "unreachable(ltypeOf) " <> pShow t

data Constant = Bool Bool
              | Int32 Int32
              | Int64 Int64
              | Word8 Word8
              | Word32 Word32
              | Word64 Word64
              | Float64 Double
              | String String
  deriving stock (Eq, Show)

instance Pretty Constant where
  pPrint = text . toString . pShow

instance HasLType Constant where
  ltypeOf Bool{}    = Bit
  ltypeOf Int32{}   = I32
  ltypeOf Int64{}   = I64
  ltypeOf Word8{}   = U8
  ltypeOf Word32{}  = U32
  ltypeOf Word64{}  = U64
  ltypeOf Float64{} = F64
  ltypeOf String{}  = Ptr U8

data Op = ADD  | SUB  | MUL  | SDIV | SREM | UDIV | UREM
        | FADD | FSUB | FMUL | FDIV
        | IEQ | INE
        | SLT | SGT | SLE | SGE
        | ULT | UGT | ULE | UGE
        | FEQ | FNE
        | FLT | FGT | FLE | FGE
        | AND | OR
  deriving stock (Eq, Show)

instance Pretty Op where
  pPrint = text . toString . pShow

ltypeOfOp :: Op -> LType -> LType
ltypeOfOp ADD  I32 = I32
ltypeOfOp ADD  I64 = I64
ltypeOfOp SUB  I32 = I32
ltypeOfOp SUB  I64 = I64
ltypeOfOp MUL  I32 = I32
ltypeOfOp MUL  I64 = I64
ltypeOfOp SDIV I32 = I32
ltypeOfOp SDIV I64 = I64
ltypeOfOp UDIV U32 = I32
ltypeOfOp UDIV U64 = I64
ltypeOfOp FADD _   = F64
ltypeOfOp FSUB _   = F64
ltypeOfOp FMUL _   = F64
ltypeOfOp FDIV _   = F64
ltypeOfOp IEQ  _   = Bit
ltypeOfOp INE  _   = Bit
ltypeOfOp SLT  I32 = Bit
ltypeOfOp SLT  I64 = Bit
ltypeOfOp SGT  I32 = Bit
ltypeOfOp SGT  I64 = Bit
ltypeOfOp SLE  I32 = Bit
ltypeOfOp SLE  I64 = Bit
ltypeOfOp SGE  I32 = Bit
ltypeOfOp SGE  I64 = Bit
ltypeOfOp ULT  U32 = Bit
ltypeOfOp ULT  U64 = Bit
ltypeOfOp UGT  U32 = Bit
ltypeOfOp UGT  U64 = Bit
ltypeOfOp ULE  U32 = Bit
ltypeOfOp ULE  U64 = Bit
ltypeOfOp UGE  U32 = Bit
ltypeOfOp UGE  U64 = Bit
ltypeOfOp FEQ  _   = Bit
ltypeOfOp FNE  _   = Bit
ltypeOfOp FLT  _   = Bit
ltypeOfOp FLE  _   = Bit
ltypeOfOp FGT  _   = Bit
ltypeOfOp FGE  _   = Bit
ltypeOfOp AND  _   = Bit
ltypeOfOp OR   _   = Bit
ltypeOfOp op   t   = error $ toText $ "unreachable(ltypeOfOp):" <> pShow op <> " " <> pShow t

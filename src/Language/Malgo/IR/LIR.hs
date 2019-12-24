{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ViewPatterns          #-}
module Language.Malgo.IR.LIR where

import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.LType
import           Relude                       hiding (Op, Type)

data Program a = Program { functions :: [Func a], mainFunc :: a }
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

instance (HasLType a, Pretty a) => Pretty (Program a) where
  pPrint Program{ functions, mainFunc } =
    "program" <+> pPrint mainFunc
    $$ vcat (map pPrint functions)

data Func a = Func { name :: a, params :: [a], body :: Block a }
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

instance (HasLType a, Pretty a) => Pretty (Func a) where
  pPrint Func{ name, params, body } =
    "func" <+> pPrint name <> "<" <> pPrint (ltypeOf name) <> ">" <> parens (sep $ punctuate "," $ map pPrint params) $$ nest 1 (pPrint body)

instance HasLType a => HasLType (Func a) where
  ltypeOf Func { name } = ltypeOf name

data Block a = Block { insts :: [(a, Inst a)], value :: a }
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

instance Pretty a => Pretty (Block a) where
  pPrint Block{ insts, value } = brackets $ vcat (punctuate ";" $ map pPrint insts) $$ pPrint value

instance HasLType a => HasLType (Block a) where
  ltypeOf Block{ value = x } = ltypeOf x

data Inst a = Constant Constant
            | Call a [a]
            | CallExt Text LType [a]
            | Alloca LType (Maybe a)
            | LoadC a [Int]
            | Load a a
            | StoreC a [Int] a
            | Store a [a] a
            | Cast LType a
            | Undef LType
            | BinOp Op a a
            | If a (Block a) (Block a)
  deriving (Eq, Show, Read, Generic, Functor, Foldable)

instance Pretty a => Pretty (Inst a) where
  pPrint (Constant c) = pPrint c
  pPrint (Call f xs) = pPrint f <> parens (sep $ punctuate "," $ map pPrint xs)
  pPrint (CallExt f t xs) = pPrint f <> "<" <> pPrint t <> ">" <> parens (sep $ punctuate "," $ map pPrint xs)
  pPrint (Alloca t s) = "alloca" <+> pPrint t <+> pPrint s
  pPrint (LoadC x is) = "loadc" <+> pPrint x <+> pPrint is
  pPrint (Load x i) = "load" <+> pPrint x <+> pPrint i
  pPrint (StoreC x is v) = "storec" <+> pPrint x <+> pPrint is <+> pPrint v
  pPrint (Store x is v) = "store" <+> pPrint x <+> pPrint is <+> pPrint v
  pPrint (Cast t a) = "cast" <+> pPrint t <+> pPrint a
  pPrint (Undef t) = "undef" <+> pPrint t
  pPrint (BinOp op x y) = pPrint op <+> pPrint x <+> pPrint y
  pPrint (If c t f) = "if" <+> pPrint c
    $$ ("then:" <+> pPrint t)
    $$ ("else:" <+> pPrint f)

instance (HasLType a, Show a) => HasLType (Inst a) where
  ltypeOf (Constant x) = ltypeOf x
  ltypeOf (Call (ltypeOf -> Function t _) _) = t
  ltypeOf (CallExt _ (ltypeOf -> Function t _) _) = t
  ltypeOf (Alloca t _) = Ptr t
  ltypeOf (LoadC x is) = accessType (ltypeOf x) is
  ltypeOf (Load (ltypeOf -> Ptr t) _) = t
  ltypeOf StoreC{} = Ptr (Struct [])
  ltypeOf Store{} = Ptr (Struct [])
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
              | String [Word8]
  deriving (Eq, Show, Read, Generic)

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
  deriving (Eq, Show, Read, Generic)

instance Pretty Op where
  pPrint = text . toString . pShow

ltypeOfOp :: Op -> LType -> LType
ltypeOfOp ADD I32  = I32
ltypeOfOp ADD I64  = I64
ltypeOfOp SUB I32  = I32
ltypeOfOp SUB I64  = I64
ltypeOfOp MUL I32  = I32
ltypeOfOp MUL I64  = I64
ltypeOfOp SDIV I32 = I32
ltypeOfOp SDIV I64 = I64
ltypeOfOp UDIV U32 = I32
ltypeOfOp UDIV U64 = I64
ltypeOfOp FADD _   = F64
ltypeOfOp FSUB _   = F64
ltypeOfOp FMUL _   = F64
ltypeOfOp FDIV _   = F64
ltypeOfOp IEQ _    = Bit
ltypeOfOp INE _    = Bit
ltypeOfOp SLT I32  = I32
ltypeOfOp SLT I64  = I64
ltypeOfOp SGT I32  = I32
ltypeOfOp SGT I64  = I64
ltypeOfOp SLE I32  = I32
ltypeOfOp SLE I64  = I64
ltypeOfOp SGE I32  = I32
ltypeOfOp SGE I64  = I64
ltypeOfOp ULT U32  = U32
ltypeOfOp ULT U64  = U64
ltypeOfOp UGT U32  = U32
ltypeOfOp UGT U64  = U64
ltypeOfOp ULE U32  = U32
ltypeOfOp ULE U64  = U64
ltypeOfOp UGE U32  = U32
ltypeOfOp UGE U64  = U64
ltypeOfOp FEQ _    = Bit
ltypeOfOp FNE _    = Bit
ltypeOfOp FLT _    = Bit
ltypeOfOp FLE _    = Bit
ltypeOfOp FGT _    = Bit
ltypeOfOp FGE _    = Bit
ltypeOfOp AND _    = Bit
ltypeOfOp OR _     = Bit
ltypeOfOp op t = error $ toText $ "unreachable(ltypeOfOp):" <> pShow op <> " " <> pShow t

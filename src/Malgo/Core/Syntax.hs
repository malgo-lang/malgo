{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Malgo.Core.Syntax where

import Control.Lens (Lens')
import Control.Lens.TH
import Koriel.Id
import Malgo.Prelude
import Malgo.TypeRep (PrimT, Rep)

type Kind = Type

data TyCon
  = TyCon Name
  | TupleC [Kind]
  deriving stock (Show, Eq, Ord)

data Type
  = TyConApp TyCon [Type]
  | TyVar Name
  | TyFun Type Type
  | TyPrim PrimT
  | TyPtr Type
  | TyBottom
  | -- | star
    TYPE Rep
  | TyForall Name Type
  deriving stock (Show, Eq, Ord)

type Name = Id Type

-- unboxed primitive values
data Unboxed = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String Text
  deriving stock (Show, Eq, Ord)

data Exp
  = Var Name
  | Unboxed Unboxed
  | Apply Exp [Exp]
  | Fn [Name] Exp
  | Match Exp [Clause]
  | Tuple [Exp]
  | Record (HashMap Name Exp)
  | RecordAccess Name
  | Type Type
  deriving stock (Show, Eq, Ord)

data Clause = Clause Pat Exp
  deriving stock (Show, Eq, Ord)

data Pat
  = VarP Name
  | ConP Name [Pat]
  | TupleP [Pat]
  | RecordP (HashMap Name Pat)
  | UnboxedP Unboxed
  deriving stock (Show, Eq, Ord)

data TypeDef = TypeDef
  { _parameters :: [Name],
    _constructors :: [Name]
  }
  deriving stock (Show, Eq, Ord)

data Module = Module
  { _variableDefinitions :: [(Name, Exp)],
    _externalDefinitions :: [(Name, String)],
    _typeDefinitions :: [(Name, TypeDef)]
  }
  deriving stock (Show, Eq, Ord)

externalDefinitions :: Lens' Module [(Name, String)]
externalDefinitions f_a5vqN (Module x1_a5vqO x2_a5vqP x3_a5vqQ) =
  fmap
    (\y1_a5vqR -> Module x1_a5vqO y1_a5vqR x3_a5vqQ)
    (f_a5vqN x2_a5vqP)
{-# INLINE externalDefinitions #-}

typeDefinitions :: Lens' Module [(Name, TypeDef)]
typeDefinitions f_a5vqS (Module x1_a5vqT x2_a5vqU x3_a5vqV) =
  fmap
    (Module x1_a5vqT x2_a5vqU)
    (f_a5vqS x3_a5vqV)
{-# INLINE typeDefinitions #-}

variableDefinitions :: Lens' Module [(Name, Exp)]
variableDefinitions f_a5vqX (Module x1_a5vqY x2_a5vqZ x3_a5vr0) =
  fmap
    (\y1_a5vr1 -> Module y1_a5vr1 x2_a5vqZ x3_a5vr0)
    (f_a5vqX x1_a5vqY)
{-# INLINE variableDefinitions #-}

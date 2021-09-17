module Malgo.Core.Syntax where

import Control.Lens (Lens')
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
externalDefinitions f (Module x1 x2 x3) = fmap (\y1 -> Module x1 y1 x3) (f x2)
{-# INLINE externalDefinitions #-}

typeDefinitions :: Lens' Module [(Name, TypeDef)]
typeDefinitions f (Module x1 x2 x3) = fmap (Module x1 x2) (f x3)
{-# INLINE typeDefinitions #-}

variableDefinitions :: Lens' Module [(Name, Exp)]
variableDefinitions f (Module x1 x2 x3) = fmap (\y1 -> Module y1 x2 x3) (f x1)
{-# INLINE variableDefinitions #-}

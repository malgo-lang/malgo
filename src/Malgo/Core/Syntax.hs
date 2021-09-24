module Malgo.Core.Syntax where

import Control.Lens (Lens', lens)
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
  | Let Name Exp Exp
  | Match [Exp] [Clause]
  | Tuple [Exp]
  | Record (HashMap Name Exp)
  | RecordAccess Name
  | Type Type
  deriving stock (Show, Eq, Ord)

data Clause = Clause [Pat] Exp
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
  { _moduleName :: ModuleName,
    _variableDefinitions :: [(Name, Exp)],
    _externalDefinitions :: [(Name, String)],
    _typeDefinitions :: [(Name, TypeDef)]
  }
  deriving stock (Show, Eq, Ord)

moduleName :: Lens' Module ModuleName
moduleName = lens _moduleName \m x -> m {_moduleName = x}
{-# INLINE moduleName #-}

variableDefinitions :: Lens' Module [(Name, Exp)]
variableDefinitions = lens _variableDefinitions \m x -> m {_variableDefinitions = x}
{-# INLINE variableDefinitions #-}

typeDefinitions :: Lens' Module [(Name, TypeDef)]
typeDefinitions = lens _typeDefinitions \m x -> m {_typeDefinitions = x}
{-# INLINE typeDefinitions #-}

externalDefinitions :: Lens' Module [(Name, String)]
externalDefinitions = lens _externalDefinitions \m x -> m { _externalDefinitions = x}
{-# INLINE externalDefinitions #-}

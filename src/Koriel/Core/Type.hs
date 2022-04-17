{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.Type where

import Control.Lens (Prism', Traversal', prism, traverseOf, traversed, (^.))
import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Binary (Binary)
import Data.Data (Data)
import Koriel.Id
import Koriel.Lens
import Koriel.Prelude
import Koriel.Pretty

{-
Constructors  C ::= <tag n>
-}
data Tag
  = Data Text
  | Tuple
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary Tag

instance Pretty Tag where
  pPrint (Data name) = pPrint name
  pPrint Tuple = "#tuple"

data Con = Con Tag [Type]
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

instance Binary Con

instance Pretty Con where
  pPrint (Con tag xs) = parens $ sep $ pPrint tag : map pPrint xs

-- TODO: クロージャを表す型を追加
-- ClosureT [Type] Type
-- あるいはknown関数を表す型
-- FuncT [Type] Type
data Type
  = [Type] :-> Type
  | Int32T
  | Int64T
  | FloatT
  | DoubleT
  | CharT
  | StringT
  | BoolT
  | SumT [Con]
  | PtrT Type
  | AnyT
  | VoidT
  deriving stock (Eq, Show, Ord, Generic, Data, Typeable)

_SumT :: Prism' Type [Con]
_SumT = prism SumT \case
  SumT cs -> Right cs
  t -> Left t

instance Binary Type

instance Pretty Type where
  pPrint (a :-> b) = sep [brackets (sep $ punctuate "," $ map pPrint a), "->", pPrint b]
  pPrint Int32T = "Int32#"
  pPrint Int64T = "Int64#"
  pPrint FloatT = "Float#"
  pPrint DoubleT = "Double#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"
  pPrint BoolT = "Bool#"
  pPrint (SumT cs) = parens $ sep ("sum" : map pPrint cs)
  pPrint (PtrT t) = parens $ "Ptr#" <+> pPrint t
  pPrint AnyT = "Any#"
  pPrint VoidT = "Void#"

class HasType a where
  typeOf :: a -> Type

instance HasType Type where
  typeOf x = x

instance HasType a => HasType (Id a) where
  typeOf x = typeOf $ x ^. idMeta

tyVar :: Traversal' Type Type
tyVar f = \case
  ps :-> r -> (:->) <$> traverseOf (traversed . tyVar) f ps <*> traverseOf tyVar f r
  SumT cs -> SumT <$> traverseOf (traversed . tyVar') f (toList cs)
  AnyT -> f AnyT
  t -> pure t

tyVar' :: Traversal' Con Type
tyVar' f (Con tag ts) = Con tag <$> traverseOf (traversed . tyVar) f ts

-- Return the most specific type
mostSpecific :: Type -> Type -> Type
mostSpecific AnyT t = t
mostSpecific t AnyT = t
mostSpecific (ps1 :-> r1) (ps2 :-> r2) = zipWith mostSpecific ps1 ps2 :-> mostSpecific r1 r2
mostSpecific (PtrT t1) (PtrT t2) = PtrT $ mostSpecific t1 t2
mostSpecific t _ = t

newtype TypeEnv = TypeEnv
  { _constructorInfoMap :: Map Text ConInfo
  }
  deriving stock (Show)
  deriving newtype (Monoid, Semigroup)

data ConInfo = ConInfo
  { _paramTypes :: [Type],
    _returnType :: Type
  }
  deriving stock (Show)

makeFieldsNoPrefix ''ConInfo
makeFieldsNoPrefix ''TypeEnv

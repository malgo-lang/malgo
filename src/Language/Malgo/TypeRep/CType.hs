{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.TypeRep.CType where

import Data.Set (fromList)
import Data.Text (pack, unpack)
import Language.Malgo.Id
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import qualified Language.Malgo.TypeRep.Type as T
import Text.PrettyPrint (punctuate)
import Text.PrettyPrint.HughesPJ
  ( braces,
    brackets,
    parens,
    sep,
    text,
  )

data CType
  = [CType] :-> CType
  | IntT
  | FloatT
  | CharT
  | StringT
  | DataT Text [CType]
  | SumT (Set Con)
  | ArrayT CType
  | VarT Int
  deriving stock (Eq, Show, Ord)

instance Pretty CType where
  pPrint (a :-> b) = parens (sep $ map pPrint a) <+> "->" <+> pPrint b
  pPrint IntT = "Int#"
  pPrint FloatT = "Float#"
  pPrint CharT = "Char#"
  pPrint StringT = "String#"
  pPrint (DataT n ts) = parens $ pPrint n <+> sep (map pPrint ts)
  pPrint (SumT cs) = braces $ sep (map pPrint $ toList cs)
  pPrint (ArrayT t) = brackets $ pPrint t
  pPrint (VarT i) = pPrint i

{-
Constructors  C ::= <tag n>
-}
type Tag = Text

data Con = Con Tag [CType]
  deriving stock (Eq, Show, Ord)

instance Pretty Con where
  pPrint (Con tag xs) = "<" <> text (unpack tag) <+> sep (punctuate "," (map pPrint xs)) <> ">"

class HasCType a where
  cTypeOf :: HasCallStack => a -> CType

instance HasCType CType where
  cTypeOf x = x

instance HasCType a => HasCType (Id a) where
  cTypeOf x = cTypeOf $ x ^. idMeta

instance HasCType T.Type where
  cTypeOf (T.TyApp T.IntC []) = SumT [Con "Int" [IntT]]
  cTypeOf (T.TyApp T.FloatC []) = SumT [Con "Float" [FloatT]]
  cTypeOf (T.TyApp T.BoolC []) = SumT [Con "True" [], Con "False" []]
  cTypeOf (T.TyApp T.CharC []) = SumT [Con "Char" [CharT]]
  cTypeOf (T.TyApp T.StringC []) = SumT [Con "String" [StringT]]
  cTypeOf (T.TyApp T.TupleC xs) = SumT [Con ("Tuple" <> pack (show $ length xs)) (map cTypeOf xs)]
  cTypeOf (T.TyApp T.ArrayC [x]) = ArrayT (cTypeOf x)
  cTypeOf (T.TyApp _ _) = bug Unreachable
  cTypeOf (T.TyMeta i) = VarT i
  cTypeOf (ps T.:-> r) = map cTypeOf ps :-> cTypeOf r

tyVar :: Applicative f => (CType -> f CType) -> CType -> f CType
tyVar f (ps :-> r) = (:->) <$> traverse (tyVar f) ps <*> tyVar f r
tyVar f (DataT n ts) = DataT n <$> traverse (tyVar f) ts
tyVar f (SumT cs) = SumT . fromList <$> traverse (tyVar' f) (toList cs)
tyVar f (ArrayT t) = ArrayT <$> tyVar f t
tyVar f (VarT x) = f (VarT x)
tyVar _ t = pure t

tyVar' :: Applicative f => (CType -> f CType) -> Con -> f Con
tyVar' f (Con tag ts) = Con tag <$> traverse (tyVar f) ts

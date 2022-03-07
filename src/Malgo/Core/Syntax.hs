module Malgo.Core.Syntax where

import Control.Lens (Lens', lens, (^.))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import Malgo.TypeRep (PrimT (..), Rep (..))

type Name = Id Type

type Kind = Type

data TyCon
  = TyCon Name
  | TupleC [Kind]
  deriving stock (Show, Eq, Ord)

instance Pretty TyCon where
  pPrint (TyCon name) = pPrint name
  pPrint (TupleC ks) = parens $ sep $ replicate (length ks) ","

data Type
  = TyConApp TyCon [Type]
  | TyRecord (HashMap Name Type)
  | TyVar Name
  | TyFun Type Type
  | TyPrim PrimT
  | TyPtr Type
  | TyBottom
  | -- | star
    TYPE Rep
  | TyForall Name Type
  deriving stock (Show, Eq, Ord)

instance Pretty Type where
  pPrint (TyConApp con ts) = pPrint con <+> sep (map (parens . pPrint) ts)
  pPrint (TyRecord kts) = braces $ sep $ punctuate "," $ map (\(k, t) -> pPrint k <> ":" <+> pPrint t) $ HashMap.toList kts
  pPrint (TyVar name) = pPrint name
  pPrint (TyFun t1 t2) = parens (pPrint t1) <+> "->" <+> parens (pPrint t2)
  pPrint (TyPrim p) = pPrint p
  pPrint (TyPtr t) = "Ptr#" <+> parens (pPrint t)
  pPrint TyBottom = "⊥"
  pPrint (TYPE rep) = "TYPE" <+> parens (pPrint rep)
  pPrint (TyForall v t) = "∀" <> pPrint v <> "." <+> pPrint t

class HasType a where
  typeOf :: a -> Type

kindOf :: Type -> Kind
kindOf (TyConApp _ _) = TYPE BoxedRep
kindOf (TyRecord _) = TYPE BoxedRep
kindOf (TyVar n) = n ^. idMeta
kindOf (TyFun _ _) = TYPE BoxedRep
kindOf (TyPrim Int32T) = TYPE Int32Rep
kindOf (TyPrim Int64T) = TYPE Int64Rep
kindOf (TyPrim FloatT) = TYPE FloatRep
kindOf (TyPrim DoubleT) = TYPE DoubleRep
kindOf (TyPrim CharT) = TYPE CharRep
kindOf (TyPrim StringT) = TYPE StringRep
kindOf (TyPtr _) = TYPE BoxedRep
kindOf TyBottom = TYPE BoxedRep
kindOf (TYPE rep) = TYPE rep
kindOf (TyForall _ t) = kindOf t

applyType :: Type -> [Type] -> Type
applyType (TyForall v t1) (t2 : ts) = applyType (applySubst [(v, t2)] t1) ts
applyType (TyFun _ ret) (_ : ts) = applyType ret ts
applyType t (_ : _) = errorDoc $ pPrint t <+> "is not applicable"
applyType t [] = t

applySubst :: [(Name, Type)] -> Type -> Type
applySubst [] t = t
applySubst subst (TyConApp con ts) = TyConApp con $ map (applySubst subst) ts
applySubst subst (TyRecord kts) = TyRecord $ fmap (applySubst subst) kts
applySubst subst (TyVar n) = case List.lookup n subst of
  Nothing -> TyVar n
  Just t -> t
applySubst subst (TyFun t1 t2) = TyFun (applySubst subst t1) (applySubst subst t2)
applySubst _ (TyPrim prim) = TyPrim prim
applySubst subst (TyPtr t) = TyPtr (applySubst subst t)
applySubst _ TyBottom = TyBottom
applySubst _ (TYPE rep) = TYPE rep
applySubst subst (TyForall v t) = TyForall v (applySubst subst t)

-- unboxed primitive values
data Unboxed = Int32 Int32 | Int64 Int64 | Float Float | Double Double | Char Char | String Text
  deriving stock (Show, Eq, Ord)

instance Pretty Unboxed where
  pPrint (Int32 x) = show x
  pPrint (Int64 x) = show x
  pPrint (Float x) = show x
  pPrint (Double x) = show x
  pPrint (Char x) = show x
  pPrint (String x) = text $ toString x

instance HasType Unboxed where
  typeOf Int32 {} = TyPrim Int32T
  typeOf Int64 {} = TyPrim Int64T
  typeOf Float {} = TyPrim FloatT
  typeOf Double {} = TyPrim DoubleT
  typeOf Char {} = TyPrim CharT
  typeOf String {} = TyPrim StringT

data Exp
  = Var Name
  | Unboxed Unboxed
  | Apply Exp [Exp]
  | Fn [Name] Exp
  | Let Name Exp Exp
  | Match [Exp] [Clause]
  | Switch Exp [Case]
  | Tuple [Exp]
  | Record (HashMap Name Exp)
  | RecordAccess (HashMap Name Type) Name
  | Type Type
  | RaiseFail Type
  | HandleFail Exp Exp
  deriving stock (Show, Eq, Ord)

instance Pretty Exp where
  pPrint (Var name) = pPrint name
  pPrint (Unboxed x) = pPrint x
  pPrint (Apply f xs) = parens $ pPrint f <+> sep (map pPrint xs)
  pPrint (Fn ps e) = braces $ sep (map (\p -> pPrint p <> ":" <> pPrint (p ^. idMeta)) ps) <+> "->" <+> pPrint e
  pPrint (Let x v e) = sep ["let" <+> pPrint x <> ":" <> pPrint (x ^. idMeta) <+> "=" <+> pPrint v, "in" <+> pPrint e]
  pPrint (Match us cs) = sep ["match" <+> pPrint us, "{", sep $ punctuate "," $ map (nest 2 . pPrint) cs, "}"]
  pPrint (Switch u cs) = sep ["switch" <+> pPrint u, "{", sep $ punctuate "," $ map (nest 2 . pPrint) cs, "}"]
  pPrint (Tuple es) = parens $ sep $ punctuate "," $ map pPrint es
  pPrint (Record kes) = braces $ sep $ punctuate "," $ map (\(k, e) -> pPrint k <> ":" <+> pPrint e) $ HashMap.toList kes
  pPrint (RecordAccess _ n) = "#" <> pPrint n
  pPrint (Type t) = "@" <> parens (pPrint t)
  pPrint (RaiseFail _) = "raise Fail"
  pPrint (HandleFail e1 e2) = parens $ sep [pPrint e1, "handle Fail =>", pPrint e2]

instance HasType Exp where
  typeOf (Var n) = n ^. idMeta
  typeOf (Unboxed u) = typeOf u
  typeOf (Apply f xs) = applyType fType xsTypes
    where
      fType = typeOf f
      xsTypes = map typeOf xs
  typeOf (Fn ps e) = buildTyFun ps (typeOf e)
    where
      buildTyFun [] t = t
      buildTyFun (p@Id {_idMeta = TYPE _} : ps) t = TyForall p $ buildTyFun ps t
      buildTyFun (p : ps) t = TyFun (p ^. idMeta) (buildTyFun ps t)
  typeOf (Let _ _ e) = typeOf e
  typeOf (Match _ []) = TyBottom
  typeOf (Match _ (Clause _ e : _)) = typeOf e
  typeOf (Switch _ []) = TyBottom
  typeOf (Switch _ (Case _ e : _)) = typeOf e
  typeOf (Tuple es) = TyConApp (TupleC $ map (kindOf . typeOf) es) (map typeOf es)
  typeOf (Record kes) = TyRecord $ fmap typeOf kes
  typeOf (RecordAccess kts n) = TyRecord kts `TyFun` HashMap.lookupDefault (error "invalid record access") n kts
  typeOf (Type t) = t
  typeOf (RaiseFail t) = t
  typeOf (HandleFail e1 _) = typeOf e1

data Clause = Clause [Pat] Exp
  deriving stock (Show, Eq, Ord)

instance Pretty Clause where
  pPrint (Clause pat e) = hsep (map pPrint pat) <+> "->" <+> pPrint e

instance HasType Clause where
  typeOf (Clause _ e) = typeOf e

data Pat
  = VarP Name
  | ConP Name [Pat]
  | TupleP [Pat]
  | RecordP (HashMap Name Pat)
  | UnboxedP Unboxed
  deriving stock (Show, Eq, Ord)

instance Pretty Pat where
  pPrint (VarP n) = pPrint n
  pPrint (ConP c []) = pPrint c
  pPrint (ConP c ps) = hsep $ pPrint c : map pPrint ps
  pPrint (TupleP ps) = parens $ hsep $ punctuate "," $ map pPrint ps
  pPrint (RecordP kps) = braces $ sep $ punctuate "," $ map (\(k, p) -> pPrint k <> ":" <+> pPrint p) $ HashMap.toList kps
  pPrint (UnboxedP u) = pPrint u

instance HasType Pat where
  typeOf (VarP n) = n ^. idMeta
  typeOf (ConP c ps) = applyType (c ^. idMeta) (map typeOf ps)
  typeOf (TupleP ps) = TyConApp (TupleC $ map (kindOf . typeOf) ps) $ map typeOf ps
  typeOf (RecordP kps) = TyRecord $ fmap typeOf kps
  typeOf (UnboxedP u) = typeOf u

data Case = Case Tag Exp
  deriving stock (Show, Eq, Ord)

instance Pretty Case where
  pPrint (Case tag e) = pPrint tag <+> "->" <+> pPrint e

instance HasType Case where
  typeOf (Case _ e) = typeOf e

data Tag
  = Default
  | ConT Name [Name]
  | TupleT [Name]
  | RecordT (HashMap Name Name)
  | UnboxedT Unboxed
  deriving stock (Show, Eq, Ord)

instance Pretty Tag where
  pPrint Default = "default"
  pPrint (ConT c []) = pPrint c
  pPrint (ConT c vs) = hsep $ pPrint c : map pPrint vs
  pPrint (TupleT vs) = parens $ hsep $ punctuate "," $ map pPrint vs
  pPrint (RecordT kvs) = braces $ sep $ punctuate "," $ map (\(k, v) -> pPrint k <> ":" <+> pPrint v) $ HashMap.toList kvs
  pPrint (UnboxedT u) = pPrint u

data TypeDef = TypeDef
  { _parameters :: [Name],
    _constructors :: [Name]
  }
  deriving stock (Show, Eq, Ord)

instance Pretty TypeDef where
  pPrint TypeDef {..} = "∀" <> hsep (map pPrint _parameters) <> "." <+> sep (punctuate " |" $ map (\c -> pPrint c <> ":" <> pPrint (c ^. idMeta)) _constructors)

data Module = Module
  { _moduleName :: ModuleName,
    _variableDefinitions :: [(Name, Exp)],
    _externalDefinitions :: [(Name, String)],
    _typeDefinitions :: [(Name, TypeDef)]
  }
  deriving stock (Show, Eq, Ord)

instance Pretty Module where
  pPrint Module {..} =
    sep
      [ "module" <+> pPrint _moduleName <+> "=" <+> "{",
        nest 2 $ sep $ punctuate ";" $ map (\(n, e) -> sep [pPrint n <> ":" <> pPrint (n ^. idMeta) <+> "=", nest 2 $ pPrint e]) _variableDefinitions,
        nest 2 $ sep $ punctuate ";" $ map (\(n, e) -> sep [pPrint n <+> "=", nest 2 $ pPrint e]) _externalDefinitions,
        nest 2 $ sep $ punctuate ";" $ map (\(n, e) -> sep [pPrint n <+> "=", nest 2 $pPrint e]) _typeDefinitions,
        "}"
      ]

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
externalDefinitions = lens _externalDefinitions \m x -> m {_externalDefinitions = x}
{-# INLINE externalDefinitions #-}

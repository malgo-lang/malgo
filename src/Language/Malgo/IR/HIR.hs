{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Malgo.IR.HIR where

import           Control.Lens                (view, _1)
import           Data.Set                    (delete, (\\))
import           Language.Malgo.ID
import           Language.Malgo.Pretty
import           Language.Malgo.TypeRep.Type
import           Relude
import           Relude.Unsafe               ((!!))

data Expr t = Var (ID t)
            | Lit Lit
            | Tuple [ID t]
            | TupleAccess (ID t) Int
            | MakeArray
              t -- type of element
              (ID t) -- size
            | ArrayRead
              (ID t) -- array
              (ID t) -- index
            | ArrayWrite
              (ID t) -- array
              (ID t) -- index
              (ID t) -- value
            | Call (ID t) [ID t]
            | Let (ID t) (Expr t) (Expr t)
            | LetRec [(ID t, [ID t], Expr t)] (Expr t)
            | If (ID t) (Expr t) (Expr t)
            | Prim Text t [ID t]
            deriving (Eq, Show, Read, Generic, PrettyVal)

data Lit = Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String Text
  deriving (Eq, Show, Read, Generic, PrettyVal)

flattenExpr :: Expr t -> Expr t
flattenExpr (Let x v1 e1) =
  insert (flattenExpr v1)
  where insert (Let y v2 e2) = Let y v2 (insert e2)
        insert (LetRec xs e) = LetRec xs (insert e)
        insert v             = Let x v (flattenExpr e1)
flattenExpr (LetRec defs body) =
  LetRec (map flattenDef defs) (flattenExpr body)
flattenExpr (If c t f) = If c (flattenExpr t) (flattenExpr f)
flattenExpr e = e

flattenDef :: (ID t, [ID t], Expr t) -> (ID t, [ID t], Expr t)
flattenDef (f, ps, e) = (f, ps, flattenExpr e)

freevars :: Ord t => Expr t -> Set (ID t)
freevars (Var x)             = one x
freevars Lit{}               = mempty
freevars (Tuple xs)          = fromList xs
freevars (TupleAccess x _) = one x
freevars (MakeArray _ x)     = one x
freevars (ArrayRead x y)     = fromList [x, y]
freevars (ArrayWrite x y z)  = fromList [x, y, z]
freevars (Call x xs)         = fromList $ x:xs
freevars (Let x v e)         = freevars v <> delete x (freevars e)
freevars (LetRec xs e) =
  let efv = freevars e
      xsfv = mconcat $ map (\(_, ps, b) -> freevars b \\ fromList ps) xs
      fs = fromList $ map (view _1) xs
  in (efv <> xsfv) \\ fs
freevars (If c t f) = one c <> freevars t <> freevars f
freevars (Prim _ _ xs) = fromList xs

instance HasType t => HasType (Expr t) where
  typeOf (Var x) = typeOf x
  typeOf (Lit x) = typeOf x
  typeOf (Tuple xs) = TyTuple $ map typeOf xs
  typeOf (TupleAccess x i) =
    case typeOf x of
      TyTuple xs -> xs !! i
      _          -> error "(typeOf e) should match (TyTuple xs)"
  typeOf (MakeArray t _) = TyArray $ typeOf t
  typeOf (ArrayRead arr _) =
    case typeOf arr of
      TyArray t -> t
      _         -> error "(typeOf arr) should match (TyArray xs)"
  typeOf ArrayWrite{} = TyTuple []
  typeOf (Call fn _) =
    case typeOf fn of
      (TyFun _ ty) -> ty
      _            -> error "(typeOf fn) should match (TyFun _ ty)"
  typeOf (Let _ _ e) = typeOf e
  typeOf (LetRec _ e) = typeOf e
  typeOf (If _ x _) = typeOf x
  typeOf (Prim _ ty _) = typeOf ty

instance HasType Lit where
  typeOf Int{}    = TyInt
  typeOf Float{}  = TyFloat
  typeOf Bool{}   = TyBool
  typeOf Char{}   = TyChar
  typeOf String{} = TyString

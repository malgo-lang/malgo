module Malgo.Refine.Space (Space (..), subspace) where

import Koriel.Id (Id)
import Malgo.Prelude hiding (Empty, subtract)
import Malgo.Syntax (Pat (..))
import Malgo.Syntax.Extension
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep.Static
import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map
import Malgo.Refine.RefineEnv

-- | Space of values that covered by patterns
data Space
  = Empty
  | Type Type
  | Constructor (Id ()) [Space]
  | Tuple [Space]
  | Record [(Id (), Space)]
  | Union Space Space
  deriving stock (Eq, Show)

-- | whether space s1 is a subspace of space s2
subspace :: Space -> Space -> Bool
subspace s1 s2 = subtract s1 s2 == Empty

-- malgoにはsubtypingがないので、これでいいはず
-- TODO: 検証。TyVar, TyBottomが気になる
isSubTypeOf :: Type -> Type -> Bool
isSubTypeOf t1 t2 = t1 == t2

-- | subtraction of s1 and s2
subtract :: Space -> Space -> Space
subtract Empty _ = Empty
subtract s1 Empty = s1
subtract (Type t1) (Type t2) | t1 `isSubTypeOf` t2 = Empty
subtract (Constructor k1 ss) (Constructor k2 ws)
  | k1 == k2 =
    if
        | and (zipWith subspace ss ws) -> Empty
        | any (`equal` Empty) (zipWith intersection ss ws) -> Constructor k1 ss
        | otherwise -> aux [] ss ws
  where
    aux _ [] [] = Empty
    aux acc (s : ss) (w : ws) = Union (Constructor k1 (acc <> [subtract s w] <> ss)) (aux (s : acc) ss ws)
    aux _ _ _ = bug $ Unreachable "length ss == length ws"
subtract (Tuple ss) (Tuple ws)
  | and (zipWith subspace ss ws) = Empty
  | any (`equal` Empty) (zipWith intersection ss ws) = Tuple ss
  | otherwise = aux [] ss ws
  where
    aux _ [] [] = Empty
    aux acc (s : ss) (w : ws) = Union (Tuple (acc <> [subtract s w] <> ss)) (aux (s : acc) ss ws)
    aux _ _ _ = bug $ Unreachable "length ss == length ws"
subtract (Record _) (Record _) = error "not implemented"
subtract (Union s1 s2) x = Union (subtract s1 x) (subtract s2 x)
subtract x (Union s1 s2) = subtract (subtract x s1) s2
subtract a _ = a

-- | intersection of spaces
intersection :: Space -> Space -> Space
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection (Type t1) (Type t2)
  | t1 `isSubTypeOf` t2 = Type t1
  | t2 `isSubTypeOf` t1 = Type t2
  | otherwise = Empty
intersection (Union s1 s2) x = Union (intersection s1 x) (intersection s2 x)
intersection x (Union s1 s2) = Union (intersection x s1) (intersection x s2)
intersection (Constructor k1 ss) (Constructor k2 ws) | k1 == k2 = Constructor k1 (zipWith intersection ss ws)
intersection (Tuple ss) (Tuple ws) = Tuple (zipWith intersection ss ws)

-- | equality relation between spaces
equal :: Space -> Space -> Bool
equal = undefined

class HasSpace a where
  space :: RefineEnv -> a -> Space

instance HasSpace Type where
  space env (viewTyConApp -> Just (TyCon con, ts)) =
    case env ^. at con of
      Just TypeDef {_valueConstructors} -> foldr (Union . space' ts) Empty _valueConstructors
      Nothing -> bug Unreachable
    where
      space' as (k, Forall ps (splitTyArr -> (ts, _))) =
        let subst = HashMap.fromList $ zip ps as
            ss = map (space env . applySubst subst) ts
         in Constructor k ss
  space env (viewTyConApp -> Just (TyTuple {}, ts)) = Tuple (map (space env) ts)
  space _ t@TyApp {} = Type t
  space _ TyVar {} = Empty
  space _ TyCon {} = bug Unreachable
  space _ t@TyPrim {} = Type t
  space _ t@TyArr {} = Type t
  space _ TyTuple {} = bug Unreachable
  space env (TyRecord kts) = Record $ over (mapped . _2) (space env) $ Map.toList kts
  space _ TyLazy = bug Unreachable
  space _ t@TyPtr {} = Type t
  space _ t@TyBottom = Type t
  space _ _ = bug Unreachable

instance HasSpace (Pat (Malgo 'Refine))
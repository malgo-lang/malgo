module Malgo.Refine.Space (Space (..), subspace) where

import Data.Foldable.Extra (anyM)
import Koriel.Id (Id)
import Malgo.Prelude hiding (Empty, subtract)
import Malgo.Refine.RefineEnv
import Malgo.Syntax (Pat (..))
import Malgo.Syntax.Extension
import Malgo.TypeRep.Static
import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map
import System.Directory.Internal

-- | Space of values that covered by patterns
data Space
  = -- | empty space
    Empty
  | -- | type space e.g. Int32, Int32#, List a, Maybe Int32
    Type Type
  | -- | constructor space e.g. Nil, Cons a (List a), Just Int32
    Constructor (Id ()) [Space]
  | Tuple [Space]
  | Record [(Id (), Space)]
  | -- | union of spaces
    Union Space Space
  deriving stock (Eq, Show)

-- | whether space s1 is a subspace of space s2
subspace :: MonadReader RefineEnv m => Space -> Space -> m Bool
subspace s1 s2 = subtract s1 s2 >>= \s' -> pure $ s' == Empty

-- malgoにはsubtypingがないので、これでいいはず
-- TODO: 検証。TyVar, TyBottomが気になる
isSubTypeOf :: Type -> Type -> Bool
isSubTypeOf t1 t2 = t1 == t2

decomposable :: Type -> Bool
decomposable (viewTyConApp -> Just (TyCon _, _)) = True

decompose :: MonadReader RefineEnv m => Type -> m Space
decompose = undefined

-- | subtraction of s1 and s2
subtract :: MonadReader RefineEnv m => Space -> Space -> m Space
subtract Empty _ = pure Empty
subtract s1 Empty = pure s1
subtract (Type t1) (Type t2) | t1 `isSubTypeOf` t2 = pure Empty
subtract (Constructor k1 ss) (Constructor k2 ws)
  | k1 == k2 = subtract' (Constructor k1) ss ws
subtract (Tuple ss) (Tuple ws) = subtract' Tuple ss ws
subtract (Record _) (Record _) = error "not implemented"
subtract (Union s1 s2) x = Union <$> subtract s1 x <*> subtract s2 x
subtract x (Union s1 s2) = do
  s1' <- subtract x s1
  subtract s1' s2
subtract (Type t) x | decomposable t = join $ subtract <$> decompose t <*> pure x
subtract x (Type t) | decomposable t = subtract x =<< decompose t
subtract a _ = pure a

subtract' :: MonadReader RefineEnv m => ([Space] -> Space) -> [Space] -> [Space] -> m Space
subtract' k ss ws = do
  isSubSpace <- and <$> zipWithM subspace ss ws
  isEmpty <- anyM equalEmpty =<< zipWithM intersection ss ws
  if
      | isSubSpace -> pure Empty
      | isEmpty -> pure $ k ss
      | otherwise -> aux [] ss ws
  where
    aux _ [] [] = pure Empty
    aux acc (s : ss) (w : ws) = do
      s' <- subtract s w
      Union (k (acc <> [s'] <> ss)) <$> aux (s : acc) ss ws
    aux _ _ _ = bug $ Unreachable "length ss == length ws"

-- | intersection of spaces
intersection :: MonadReader RefineEnv m => Space -> Space -> m Space
intersection Empty _ = pure Empty
intersection _ Empty = pure Empty
intersection (Type t1) (Type t2)
  | t1 `isSubTypeOf` t2 = pure $ Type t1
  | t2 `isSubTypeOf` t1 = pure $ Type t2
  | otherwise = pure Empty
intersection (Union s1 s2) x = Union <$> intersection s1 x <*> intersection s2 x
intersection x (Union s1 s2) = Union <$> intersection x s1 <*> intersection x s2
intersection (Constructor k1 ss) (Constructor k2 ws) | k1 == k2 = Constructor k1 <$> zipWithM intersection ss ws
intersection (Tuple ss) (Tuple ws) = Tuple <$> zipWithM intersection ss ws
intersection (Type t) x | decomposable t = join $ intersection <$> decompose t <*> pure x
intersection x (Type t) | decomposable t = intersection x =<< decompose t
intersection _ _ = pure Empty

-- | compare with empty space
equalEmpty :: MonadReader RefineEnv m => Space -> m Bool
equalEmpty Empty = pure True
equalEmpty (Type t)
  | decomposable t = equalEmpty =<< decompose t
  | otherwise = pure False
equalEmpty (Union s1 s2) = andM (equalEmpty s1) (equalEmpty s2)
equalEmpty (Constructor _ ss) = anyM equalEmpty ss
equalEmpty (Tuple ss) = anyM equalEmpty ss
equalEmpty (Record (map snd -> ss)) = anyM equalEmpty ss

class HasSpace a where
  space :: RefineEnv -> a -> Space

instance HasSpace Type where
  space env (viewTyConApp -> Just (TyCon con, ts)) =
    case env ^. at con of
      Just TypeDef {_valueConstructors} -> foldr (Union . space' ts) Empty _valueConstructors
      Nothing -> bug $ Unreachable "con must be defined in env"
    where
      space' as (k, Forall ps (splitTyArr -> (ts, _))) =
        let subst = HashMap.fromList $ zip ps as
            ss = map (space env . applySubst subst) ts
         in Constructor k ss
  space env (viewTyConApp -> Just (TyTuple {}, ts)) = Tuple (map (space env) ts)
  space _ t@TyApp {} = Type t
  space _ TyVar {} = Empty
  space _ TyCon {} = bug $ Unreachable "`viewTyConApp (TyCon con)` returns `Just (TyCon con, [])`"
  space _ t@TyPrim {} = Type t
  space _ t@TyArr {} = Type t
  space _ TyTuple {} = bug $ Unreachable "`viewTyConApp (TyTuple n)` returns `Just (TyTuple n, [])`"
  space env (TyRecord kts) = Record $ over (mapped . _2) (space env) $ Map.toList kts
  space _ TyLazy = bug $ Unreachable "`TyLazy` does not appear except for the 1st argument of `TyApp`"
  space _ t@TyPtr {} = Type t
  space _ t@TyBottom = Type t
  space _ _ = bug $ Unreachable "All patterns are covered"

instance HasSpace (Pat (Malgo 'Refine))
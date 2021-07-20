module Malgo.Refine.Space (Space (..), subspace) where

import Koriel.Id (Id)
import Malgo.Prelude hiding (Empty, subtract)
import Malgo.Syntax (Pat (..))
import Malgo.Syntax.Extension
import Malgo.TypeCheck.TcEnv
import Malgo.TypeRep.Static

-- | Space of values that covered by patterns
data Space
  = Empty
  | PrimT PrimT
  | Constructor (Id ()) [Space]
  | Tuple [Space]
  | Record [(Id (), Space)]
  | Union Space Space
  deriving stock (Eq, Show)

-- | whether space s1 is a subspace of space s2
subspace s1 s2 = subtract s1 s2 == Empty

-- | subtraction of s1 and s2
subtract Empty _ = Empty
subtract s1 Empty = s1
subtract (Union s1 s2) x = Union (subtract s1 x) (subtract s2 x)
subtract x (Union s1 s2) = subtract (subtract x s1) s2
subtract (Constructor k1 ss) (Constructor k2 ws)
  | k1 == k2 =
    if
        | and (zipWith subspace ss ws) -> Empty
        | any (`equal` Empty) (zipWith intersection ss ws) -> Constructor k1 ss
        | otherwise -> aux [] ss ws
  where
    aux _ [] [] = Empty
    aux acc (s : ss) (w : ws) = Union (Constructor k1 (acc <> [subtract s w] <> ss)) (aux (s : acc) ss ws)
    aux _ _ _ = bug Unreachable -- length ss == length ws
subtract (Tuple ss) (Tuple ws) =
  if
      | and (zipWith subspace ss ws) -> Empty
      | any (`equal` Empty) (zipWith intersection ss ws) -> Tuple ss
      | otherwise -> aux [] ss ws
  where
    aux _ [] [] = Empty
    aux acc (s : ss) (w : ws) = Union (Tuple (acc <> [subtract s w] <> ss)) (aux (s : acc) ss ws)
    aux _ _ _ = bug Unreachable -- length ss == length ws
subtract (Record _) (Record _) = error "not implemented"
subtract a _ = a

-- | intersection of spaces
intersection :: Space -> Space -> Space
intersection = undefined

-- | equality relation between spaces
equal :: Space -> Space -> Bool
equal = undefined

class HasSpace a where
  space :: TcEnv -> a -> Space

instance HasSpace Type where

instance HasSpace (Pat (Malgo 'Refine))
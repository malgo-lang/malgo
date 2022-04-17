module Koriel.Core.Match where

import Control.Lens (has, prism')
import Control.Lens.Cons (Cons (_Cons), uncons)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Traversable (for)
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude hiding (group, uncons)
import Koriel.Pretty

-- | Compile a match expression into a switch-case like structure.
simplifyMatch :: [Exp (Id Type)] -> NonEmpty (Case (Id Type)) -> m (Exp (Id Type))
simplifyMatch = _

newtype PatMatrix = PatMatrix
  { -- | transposed matrix of patterns
    innerList :: [[Pat (Id Type)]]
  }
  deriving stock (Eq, Show)
  deriving newtype (Pretty)

patMatrix :: [[Pat (Id Type)]] -> PatMatrix
patMatrix xss = PatMatrix (transpose xss)

instance Cons PatMatrix PatMatrix [Pat (Id Type)] [Pat (Id Type)] where
  _Cons =
    prism'
      ( \(heads, matrix) ->
          matrix {innerList = heads : innerList matrix}
      )
      ( \case
          PatMatrix {innerList = []} -> Nothing
          PatMatrix {innerList = (heads : xss)} -> Just (heads, PatMatrix xss)
      )

match ::
  -- | The expression to match against. (scrutinee)
  [Exp (Id Type)] ->
  -- | Patterns to match.
  PatMatrix ->
  -- | The body of the match expression.
  [Exp (Id Type)] ->
  -- | The error expression to use if no match is found.
  Exp (Id Type) ->
  m (Exp (Id Type))
match (scrutinee : restScrutinee) pat@(uncons -> Just (heads, tails)) bodys err
  -- Variable Rule
  -- when all patterns are started with a variable pattern (Bind)
  | all (has _Bind) heads = do
    -- For all Bind v, v = scrutinee
    match restScrutinee tails (zipWith (substBind scrutinee) heads bodys) err
  -- Constructor Rule
  -- when all patterns are started with a constructor pattern (Unpack)
  | all (has _Unpack) heads = do
    -- list up all constructors
    let constructors = case typeOf scrutinee of
          SumT cs -> cs
          _ -> error "match: scrutinee is not a sum type"
    _

substBind :: Exp (Id Type) -> Pat (Id Type) -> Exp (Id Type) -> Exp (Id Type)
substBind scrutinee (Bind v) exp =
  Match [scrutinee] $ Case [Bind v] exp :| []
substBind _ _ _ = error "substBind: unreachable"

group = _

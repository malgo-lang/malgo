module Malgo.Core.Match (compileMatch) where

import Control.Lens (traverseOf, traversed, _2)
import Data.List.Extra (nubOrdBy)
import Data.Traversable (for)
import Koriel.Id
import Koriel.MonadUniq (HasUniqSupply)
import Malgo.Core.Syntax
import Malgo.Prelude
import qualified Relude.Unsafe as Unsafe

-- | convert `Match` to `Switch`
compileMatch :: (MonadIO m, MonadReader env m, HasUniqSupply env) => Module -> m Module
compileMatch Module {..} = do
  _variableDefinitions <- traverseOf (traversed . _2) cmExp _variableDefinitions
  pure Module {..}

cmExp :: (MonadIO f, HasUniqSupply env, MonadReader env f) => Exp -> f Exp
cmExp (Match us cs) = do
  us <- traverse cmExp us
  cs <- traverse cmClause cs
  match us cs
  where
    cmClause (Clause ps e) = Clause ps <$> cmExp e
cmExp (Apply e es) = Apply <$> cmExp e <*> traverse cmExp es
cmExp (Fn ps e) = Fn ps <$> cmExp e
cmExp (Let n e1 e2) = Let n <$> cmExp e1 <*> cmExp e2
cmExp (Switch e cs) = Switch <$> cmExp e <*> traverse cmCase cs
  where
    cmCase (Case tag e) = Case tag <$> cmExp e
cmExp (Tuple es) = Tuple <$> traverse cmExp es
cmExp (Record kes) = Record <$> traverse cmExp kes
cmExp (HandleFail e1 e2) = HandleFail <$> cmExp e1 <*> cmExp e2
cmExp e = pure e

-- Empty case
match :: (MonadIO f, HasUniqSupply env, MonadReader env f) => [Exp] -> [Clause] -> f Exp
match [] (Clause [] e1 : _) = pure e1
-- Variable case
match (u : us) cs | all isVarP cs = match us $ map (toLet u) cs
  where
    toLet u (Clause (VarP v : ps) e) = Clause ps (Let v u e)
    toLet _ _ = error "unreachable"
-- Constructor case
match (u : us) cs | all isConP cs = do
  -- 先頭のパターン候補を取り出す
  -- Cons 1 xs -> (Cons, [Int32, List Int32])
  let heads =
        cs
          & map
            ( \case
                Clause (ConP n ps : _) _ -> (n, map typeOf ps)
                Clause _ _ -> error "unreachable"
            )
          & nubOrdBy (\pat pat' -> fst pat `compare` fst pat')
  cases <- for heads \(name, argTypes) -> do
    args <- traverse (newInternalId "$p") argTypes
    Case (ConT name args) <$> match (map Var args <> us) (specialize name cs)
  pure $ Switch u (cases <> [Case Default (RaiseFail $ typeOf $ Unsafe.head cases)])
  where
    specialize :: Name -> [Clause] -> [Clause]
    specialize discriminant cs = mapMaybe ?? cs $ \case
      Clause (ConP con ps : restPs) e | discriminant == con -> Just $ Clause (ps <> restPs) e
      _ -> Nothing

isVarP :: Clause -> Bool
isVarP (Clause (VarP {} : _) _) = True
isVarP _ = False

isConP :: Clause -> Bool
isConP (Clause (ConP {} : _) _) = True
isConP _ = False


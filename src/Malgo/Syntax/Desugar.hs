{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Malgo.Syntax.Desugar (desugar, DesugarError) where

import Data.Traversable (for)
import Effectful.Error.Static (Error, throwError)
import Malgo.Location
import Malgo.Name
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Unique

desugar :: (Error DesugarError :> es, UniqueGen :> es) => [Definition Raw Name] -> Eff es [Definition FlattenedPatterns Name]
desugar definitions = for definitions \Definition {..} -> do
  term <- flat term
  term <- flat term
  pure Definition {..}

data DesugarError
  = Unimplemented
  | NoExhaustivePattern Location
  deriving (Show)

class Flat a r | r -> a where
  flat :: a -> r

instance (Error DesugarError :> es) => Flat (Term Raw Name) (Eff es (Term FlattenedCopatterns Name)) where
  flat Var {..} = pure Var {..}
  flat Literal {..} = pure Literal {..}
  flat Construct {..} = Construct location tag <$> traverse flat producers <*> traverse flat consumers
  flat Comatch {..} = flatCopattern location coclauses
  flat Destruct {..} = Destruct location <$> flat term <*> pure tag <*> traverse flat producers <*> traverse flat consumers
  flat Match {..} = Match location <$> flat term <*> traverse flat clauses
  flat Let {..} = Let location name <$> flat producer <*> flat term
  flat Prim {..} = Prim location tag <$> traverse flat producers <*> traverse flat consumers
  flat Switch {..} =
    Switch location
      <$> flat term
      <*> traverse (\(literal, term) -> (literal,) <$> flat term) branches
      <*> flat defaultBranch
  flat Invoke {..} = Invoke location name <$> traverse flat producers <*> traverse flat consumers
  flat Label {..} = Label location name <$> flat term
  flat Goto {..} = Goto location <$> flat term <*> pure name

instance (Error DesugarError :> es) => Flat (Clause Raw Name) (Eff es (Clause FlattenedCopatterns Name)) where
  flat Clause {..} = Clause (flat pattern) <$> flat term

instance Flat (Pattern Raw Name) (Pattern FlattenedCopatterns Name) where
  flat PConstruct {..} = PConstruct {params = map flat params, returns = map flat returns, ..}
  flat PVar {..} = PVar {..}

flatCopattern :: (Error DesugarError :> es) => Location -> [Coclause Raw Name] -> Eff es (Term FlattenedCopatterns Name)
flatCopattern location coclauses = throwError Unimplemented

instance (Error DesugarError :> es, UniqueGen :> es) => Flat (Term FlattenedCopatterns Name) (Eff es (Term FlattenedPatterns Name)) where
  flat Var {..} = pure Var {..}
  flat Literal {..} = pure Literal {..}
  flat Construct {..} = Construct location tag <$> traverse flat producers <*> traverse flat consumers
  flat Comatch {..} = Comatch location <$> traverse flat coclauses
  flat Destruct {..} = Destruct location <$> flat term <*> pure tag <*> traverse flat producers <*> traverse flat consumers
  flat Match {..} = flatPattern location term clauses
  flat Let {..} = Let location name <$> flat producer <*> flat term
  flat Prim {..} = Prim location tag <$> traverse flat producers <*> traverse flat consumers
  flat Switch {..} = Switch location <$> flat term <*> traverse (\(literal, term) -> (literal,) <$> flat term) branches <*> flat defaultBranch
  flat Invoke {..} = Invoke location name <$> traverse flat producers <*> traverse flat consumers
  flat Label {..} = Label location name <$> flat term
  flat Goto {..} = Goto location <$> flat term <*> pure name

-- Flat Coclause
instance (Error DesugarError :> es, UniqueGen :> es) => Flat (Coclause FlattenedCopatterns Name) (Eff es (Coclause FlattenedPatterns Name)) where
  flat Coclause {..} = Coclause (flat copattern) <$> flat term

-- Flat Copattern
instance Flat (Copattern FlattenedCopatterns Name) (Copattern FlattenedPatterns Name) where
  flat CDestruct {..} = CDestruct {..}

flatPattern :: (UniqueGen :> es, Error DesugarError :> es) => Location -> Term FlattenedCopatterns Name -> [Clause FlattenedCopatterns Name] -> Eff es (Term FlattenedPatterns Name)
flatPattern location term clauses = do
  term <- flat term
  name <- newName "flatPattern"
  let matrix = fromClauses clauses
  match <- compilePattern location [name] matrix
  pure $ Let location name term match

newtype PatternMatrix = PatternMatrix [([Pattern FlattenedCopatterns Name], Term FlattenedCopatterns Name)]

fromClauses :: [Clause FlattenedCopatterns Name] -> PatternMatrix
fromClauses = PatternMatrix . map (\Clause {..} -> ([pattern], term))

compilePattern :: (UniqueGen :> es, Error DesugarError :> es) => Location -> [Name] -> PatternMatrix -> Eff es (Term FlattenedPatterns Name)
compilePattern location _ (PatternMatrix []) = throwError $ NoExhaustivePattern location
compilePattern location scrutinees (variableRule -> Just (variables, term)) = do
  term <- flat term
  pure $ go scrutinees variables term
  where
    go [] [] term = term
    go (scrutinee : scrutinees) (variable : variables) term = Let location variable (Var location scrutinee) (go scrutinees variables term)
    go _ _ _ = error "impossible"
compilePattern location scrutinees (mixRule -> Just (i, constructors, matrix)) = do
  let (scrutinee, rest) = swapUncons i scrutinees
  Match location (Var location scrutinee) <$> for constructors \(tag, paramLength, returnLength) -> do
    _

variableRule :: PatternMatrix -> Maybe ([Name], Term FlattenedCopatterns Name)
variableRule = _

mixRule :: PatternMatrix -> Maybe (Int, [(Text, Int, Int)], PatternMatrix)
mixRule = _

swapUncons :: Int -> [a] -> (a, [a])
swapUncons i xs = (xs !! i, take i xs <> drop (i + 1) xs)
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Core
  ( Program (..),
    Producer (..),
    Consumer (..),
    Rank (..),
    Statement (..),
    Branch (..),
    convertToZero,
  )
where

import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module (ModuleName)
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.SExpr hiding (Char, Double, Float, String)
import Malgo.Sequent.Fun (Literal, Name, Pattern, Tag)

data Program x = Program
  {definitions :: [(Range, Name, Producer x)]}
  deriving stock (Show)

instance ToSExpr (Program x) where
  toSExpr (Program defs) = S.L $ map (\(_, name, body) -> toSExpr (name, body)) defs

type data Rank = Zero | One

data Producer (x :: Rank) where
  Var :: Range -> Name -> Producer x
  Literal :: Range -> Literal -> Producer x
  Construct :: Range -> Tag -> [Producer x] -> [Consumer x] -> Producer x
  Lambda :: Range -> [Name] -> Statement x -> Producer x
  Object :: Range -> Map Text (Statement x) -> Producer x
  Do :: Range -> Name -> Statement One -> Producer One

deriving stock instance Show (Producer x)

instance ToSExpr (Producer x) where
  toSExpr (Var _ name) = toSExpr name
  toSExpr (Literal _ literal) = toSExpr literal
  toSExpr (Construct _ tag producers consumers) =
    S.L [S.A "construct", toSExpr tag, S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Lambda _ names statement) = S.L [S.A "lambda", S.L $ map toSExpr names, toSExpr statement]
  toSExpr (Object _ kvs) = S.L $ map (\(k, v) -> S.L [toSExpr k, toSExpr v]) $ Map.toList kvs
  toSExpr (Do _ name statement) = S.L [S.A "do", toSExpr name, toSExpr statement]

data Consumer x where
  Label :: Range -> Name -> Consumer x
  Apply :: Range -> [Producer x] -> [Consumer x] -> Consumer x
  Project :: Range -> Text -> Consumer x -> Consumer x
  Then :: Range -> Name -> Statement x -> Consumer x
  Finish :: Range -> Consumer x
  Select :: Range -> [Branch x] -> Consumer x

deriving stock instance Show (Consumer x)

instance ToSExpr (Consumer x) where
  toSExpr (Label _ name) = toSExpr name
  toSExpr (Apply _ producers consumers) = S.L [S.A "apply", S.L $ map toSExpr producers, S.L $ map toSExpr consumers]
  toSExpr (Project _ field return) = S.L [S.A "project", toSExpr field, toSExpr return]
  toSExpr (Then _ name statement) = S.L [S.A "then", toSExpr name, toSExpr statement]
  toSExpr (Finish _) = S.A "finish"
  toSExpr (Select _ branches) = S.L $ S.A "select" : map toSExpr branches

data Statement x where
  Cut :: Producer x -> Consumer x -> Statement x
  CutDo :: Range -> Name -> Statement Zero -> Consumer Zero -> Statement Zero
  Primitive :: Range -> Text -> [Producer x] -> [Consumer x] -> Statement x

deriving stock instance Show (Statement x)

instance ToSExpr (Statement x) where
  toSExpr (Cut producer consumer) = S.L [toSExpr producer, S.A "|", toSExpr consumer]
  toSExpr (CutDo _ name statement consumer) = S.L [S.A "do", toSExpr name, toSExpr statement, S.A "|", toSExpr consumer]
  toSExpr (Primitive _ name producers consumers) =
    S.L [S.A "prim", toSExpr name, S.L $ map toSExpr producers, S.L $ map toSExpr consumers]

data Branch x = Branch
  { range :: Range,
    pattern :: Pattern,
    statement :: Statement x
  }

deriving stock instance Show (Branch x)

instance ToSExpr (Branch x) where
  toSExpr (Branch _ pattern statement) = S.L [toSExpr pattern, toSExpr statement]

convertToZero :: (State Uniq :> es, Reader ModuleName :> es) => Statement One -> Eff es (Statement Zero)
convertToZero x = castToZero <$> flat x

class Flat es f where
  flat :: f One -> Eff es (f One)

instance (State Uniq :> es, Reader ModuleName :> es) => Flat es Producer where
  flat :: Producer One -> Eff es (Producer One)
  flat (Var range name) = pure (Var range name)
  flat (Literal range literal) = pure (Literal range literal)
  flat (Construct range tag producers consumers) = do
    let (flatProducers, mproducer, rest) = split producers
    case mproducer of
      Just producer -> do
        label <- newTemporalId "label"
        var <- newTemporalId "var"
        producer' <- flat producer
        constructor <- flat (Construct range tag (flatProducers <> [Var range var] <> rest) consumers)
        pure
          $ Do range label
          $ Cut producer'
          $ Then range var
          $ Cut constructor
          $ Label range label
      Nothing -> do
        producers' <- traverse flat flatProducers
        consumers' <- traverse flat consumers
        pure (Construct range tag producers' consumers')
  flat (Lambda range names statement) = do
    statement' <- flat statement
    pure (Lambda range names statement')
  flat (Object range kvs) = do
    kvs' <- traverse flat kvs
    pure (Object range kvs')
  flat (Do range name statement) = do
    statement' <- flat statement
    pure (Do range name statement')

split :: [Producer One] -> ([Producer One], Maybe (Producer One), [Producer One])
split = aux []

aux :: [Producer One] -> [Producer One] -> ([Producer One], Maybe (Producer One), [Producer One])
aux acc [] = (reverse acc, Nothing, [])
aux acc (p : ps) = case p of
  Do {} -> (reverse acc, Just p, ps)
  _ -> aux (p : acc) ps

instance (State Uniq :> es, Reader ModuleName :> es) => Flat es Consumer where
  flat :: Consumer One -> Eff es (Consumer One)
  flat (Label range name) = pure (Label range name)
  flat (Apply range producers consumers) = do
    let (flatProducers, mproducer, rest) = split producers
    case mproducer of
      Just producer -> do
        outer <- newTemporalId "outer"
        inner <- newTemporalId "inner"
        producer' <- flat producer
        apply <- flat (Apply range (flatProducers <> [Var range inner] <> rest) consumers)
        pure
          $ Then range outer
          $ Cut producer'
          $ Then range inner
          $ Cut (Var range outer) apply
      Nothing -> do
        producers' <- traverse flat flatProducers
        consumers' <- traverse flat consumers
        pure (Apply range producers' consumers')
  flat (Project range field return) = do
    return' <- flat return
    pure (Project range field return')
  flat (Then range name statement) = do
    statement' <- flat statement
    pure (Then range name statement')
  flat (Finish range) = pure (Finish range)
  flat (Select range branches) = do
    branches' <- traverse flat branches
    pure (Select range branches')

instance (State Uniq :> es, Reader ModuleName :> es) => Flat es Statement where
  flat :: Statement One -> Eff es (Statement One)
  flat (Cut producer consumer) = do
    producer' <- flat producer
    consumer' <- flat consumer
    pure (Cut producer' consumer')
  flat (Primitive range name producers consumers) = do
    let (flatProducers, mproducer, rest) = split producers
    case mproducer of
      Just producer -> do
        var <- newTemporalId "var"
        producer' <- flat producer
        primitive <- flat (Primitive range name (flatProducers <> [Var range var] <> rest) consumers)
        pure $ Cut producer' $ Then range var primitive
      Nothing -> do
        producers' <- traverse flat flatProducers
        consumers' <- traverse flat consumers
        pure (Primitive range name producers' consumers')

instance (State Uniq :> es, Reader ModuleName :> es) => Flat es Branch where
  flat :: Branch One -> Eff es (Branch One)
  flat (Branch range patterns statement) = do
    statement' <- flat statement
    pure (Branch range patterns statement')

class CastToZero f where
  castToZero :: f One -> f Zero

instance CastToZero Producer where
  castToZero :: Producer One -> Producer Zero
  castToZero (Var range name) = Var range name
  castToZero (Literal range literal) = Literal range literal
  castToZero (Construct range tag producers consumers) =
    Construct range tag (fmap castToZero producers) (fmap castToZero consumers)
  castToZero (Lambda range names statement) = Lambda range names (castToZero statement)
  castToZero (Object range kvs) = Object range (fmap castToZero kvs)
  castToZero Do {} = error "castToZero: Do"

instance CastToZero Consumer where
  castToZero :: Consumer One -> Consumer Zero
  castToZero (Label range name) = Label range name
  castToZero (Apply range producers consumers) =
    Apply range (fmap castToZero producers) (fmap castToZero consumers)
  castToZero (Project range field return) = Project range field $ castToZero return
  castToZero (Then range name statement) = Then range name (castToZero statement)
  castToZero (Finish range) = Finish range
  castToZero (Select range branches) = Select range (fmap castToZero branches)

instance CastToZero Statement where
  castToZero :: Statement One -> Statement Zero
  castToZero (Cut (Do range name statement) consumer) = CutDo range name (castToZero statement) (castToZero consumer)
  castToZero (Cut producer consumer) = Cut (castToZero producer) (castToZero consumer)
  castToZero (Primitive range name producers consumers) =
    Primitive range name (fmap castToZero producers) (fmap castToZero consumers)

instance CastToZero Branch where
  castToZero :: Branch One -> Branch Zero
  castToZero (Branch range patterns statement) = Branch range patterns (castToZero statement)
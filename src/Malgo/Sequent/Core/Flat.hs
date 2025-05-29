{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Core.Flat
  ( flatProgram,
  )
where

import Control.Lens (traverseOf, _2)
import Data.Traversable (for)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module (ModuleName)
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.Sequent.Core (Branch (..), Consumer (..), Producer (..), Program (..), Rank (..), Statement (..))
import Malgo.Sequent.Fun (Name)

-- | Flattens a program into a program with no nested do expressions.
flatProgram :: (State Uniq :> es, Reader ModuleName :> es) => Program Full -> Eff es (Program Flat)
flatProgram Program {definitions, dependencies} = do
  definitions <- traverse flatDefinition definitions
  pure Program {..}

flatDefinition :: (State Uniq :> es, Reader ModuleName :> es) => (t1, t2, t3, Statement Full) -> Eff es (t1, t2, t3, Statement Flat)
flatDefinition (range, name, return, statement) = (range,name,return,) <$> flatStatement statement

-- | Wip is a temporary data structure to represent the intermediate state of the flattening process.
data Wip
  = Do' Range Name (Statement Flat)
  | Zero (Producer Flat)

cut :: Wip -> Consumer Flat -> Statement Flat
cut (Do' range name statement) consumer = Join range name consumer statement
cut (Zero producer) consumer = Cut producer consumer

flatStatement :: (State Uniq :> es, Reader ModuleName :> es) => Statement Full -> Eff es (Statement Flat)
flatStatement (Cut producer consumer) = do
  producer <- flatProducer producer
  consumer <- flatConsumer consumer
  case producer of
    Do' range name statement -> pure $ Join range name consumer statement
    Zero producer -> pure $ Cut producer consumer
flatStatement (Primitive range name producers consumer) = do
  (zeros, mproducer, rest) <- split producers
  case mproducer of
    Just producer -> do
      var <- newTemporalId "var"
      producer' <- flatProducer producer
      primitive <- flatStatement (Primitive range name (zeros <> [Var range var] <> rest) consumer)
      pure
        $ cut producer'
        $ Then range var primitive
    Nothing -> do
      producers' <- for zeros \zero -> do
        zero' <- flatProducer zero
        case zero' of
          Zero producer' -> pure producer'
          Do' {} -> error "impossible"
      consumer' <- flatConsumer consumer
      pure $ Primitive range name producers' consumer'
flatStatement (Invoke range name consumer) = do
  consumer' <- flatConsumer consumer
  pure $ Invoke range name consumer'

flatProducer :: (State Uniq :> es, Reader ModuleName :> es) => Producer Full -> Eff es Wip
flatProducer (Var range name) = pure $ Zero (Var range name)
flatProducer (Literal range literal) = pure $ Zero (Literal range literal)
flatProducer (Construct range tag producers consumers) = do
  (zeros, mproducer, rest) <- split producers
  case mproducer of
    Just producer -> do
      label <- newTemporalId "label"
      var <- newTemporalId "var"
      constructor <- flatProducer (Construct range tag (zeros <> [Var range var] <> rest) consumers)
      producer' <- flatProducer producer
      pure
        $ Do' range label
        $ cut producer'
        $ Then range var
        $ cut constructor
        $ Label range label
    Nothing -> do
      producers' <- for zeros \zero -> do
        zero' <- flatProducer zero
        case zero' of
          Zero producer' -> pure producer'
          Do' {} -> error "impossible"
      consumers' <- traverse flatConsumer consumers
      pure $ Zero (Construct range tag producers' consumers')
flatProducer (Lambda range names statement) = do
  statement <- flatStatement statement
  pure $ Zero (Lambda range names statement)
flatProducer (Object range fields) = do
  fields <- traverseOf (traverse . _2) flatStatement fields
  pure $ Zero (Object range fields)
flatProducer (Do range name statement) = do
  statement' <- flatStatement statement
  pure $ Do' range name statement'

flatConsumer :: (State Uniq :> es, Reader ModuleName :> es) => Consumer Full -> Eff es (Consumer Flat)
flatConsumer (Label range name) = pure $ Label range name
flatConsumer (Apply range producers consumers) = do
  (zeros, mproducer, rest) <- split producers
  case mproducer of
    Just producer -> do
      outer <- newTemporalId "outer"
      inner <- newTemporalId "inner"
      apply <- flatConsumer (Apply range (zeros <> [Var range inner] <> rest) consumers)
      producer' <- flatProducer producer
      pure
        $ Then range outer
        $ cut producer'
        $ Then range inner
        $ cut (Zero (Var range outer)) apply
    Nothing -> do
      producers' <- for zeros \zero -> do
        zero' <- flatProducer zero
        case zero' of
          Zero producer' -> pure producer'
          Do' {} -> error "impossible" -- zeros only contains Rank Zero producers
      consumers' <- traverse flatConsumer consumers
      pure $ Apply range producers' consumers'
flatConsumer (Project range field return) = do
  return' <- flatConsumer return
  pure $ Project range field return'
flatConsumer (Then range name statement) = do
  statement' <- flatStatement statement
  pure $ Then range name statement'
flatConsumer (Finish range) = pure $ Finish range
flatConsumer (Select range branches) = do
  branches <- traverse flatBranch branches
  pure $ Select range branches

flatBranch :: (State Uniq :> es, Reader ModuleName :> es) => Branch Full -> Eff es (Branch Flat)
flatBranch (Branch range pattern statement) = do
  statement <- flatStatement statement
  pure $ Branch range pattern statement

split :: (State Uniq :> es, Reader ModuleName :> es) => [Producer Full] -> Eff es ([Producer Full], Maybe (Producer Full), [Producer Full])
split = aux []
  where
    aux :: (State Uniq :> es, Reader ModuleName :> es) => [Producer Full] -> [Producer Full] -> Eff es ([Producer Full], Maybe (Producer Full), [Producer Full])
    aux acc [] = pure (reverse acc, Nothing, [])
    aux acc (p : ps) = do
      if isValue p
        then aux (p : acc) ps
        else pure (reverse acc, Just p, ps)

isValue :: Producer Full -> Bool
isValue Var {} = True
isValue Literal {} = True
isValue (Construct _ _ ps _) = all isValue ps
isValue Lambda {} = True
isValue Object {} = True
isValue Do {} = False
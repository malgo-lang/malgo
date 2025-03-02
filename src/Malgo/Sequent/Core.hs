{-# LANGUAGE UndecidableInstances #-}

module Malgo.Sequent.Core
  ( Program (..),
    Producer (..),
    Consumer (..),
    Rank (..),
    Statement (..),
    Branch (..),
    flatProgram,
  )
where

import Control.Lens (traverseOf, _2)
import Data.Map qualified as Map
import Data.SCargot.Repr.Basic qualified as S
import Data.Traversable (for)
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
  {definitions :: [(Range, Name, Name, Statement x)]}
  deriving stock (Show)

instance ToSExpr (Program x) where
  toSExpr (Program defs) = S.L $ map (\(_, name, return, body) -> toSExpr (name, return, body)) defs

type data Rank = Zero | One

data Producer (x :: Rank) where
  Var :: Range -> Name -> Producer x
  Literal :: Range -> Literal -> Producer x
  Construct :: Range -> Tag -> [Producer x] -> [Consumer x] -> Producer x
  Lambda :: Range -> [Name] -> Statement x -> Producer x
  Object :: Range -> Map Text (Name, Statement x) -> Producer x
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
  Primitive :: Range -> Text -> [Producer x] -> Consumer x -> Statement x
  Invoke :: Range -> Name -> Consumer x -> Statement x

deriving stock instance Show (Statement x)

instance ToSExpr (Statement x) where
  toSExpr (Cut producer consumer) = S.L ["cut", toSExpr producer, toSExpr consumer]
  toSExpr (CutDo _ name statement consumer) = S.L ["cut-do", toSExpr name, toSExpr statement, toSExpr consumer]
  toSExpr (Primitive _ name producers consumer) =
    S.L [S.A "prim", toSExpr name, S.L $ map toSExpr producers, toSExpr consumer]
  toSExpr (Invoke _ name consumer) = S.L ["invoke", toSExpr name, toSExpr consumer]

data Branch x = Branch
  { range :: Range,
    pattern :: Pattern,
    statement :: Statement x
  }

deriving stock instance Show (Branch x)

instance ToSExpr (Branch x) where
  toSExpr (Branch _ pattern statement) = S.L [toSExpr pattern, toSExpr statement]

-- | Flattens a program into a program with no nested do expressions.
flatProgram :: (State Uniq :> es, Reader ModuleName :> es) => Program One -> Eff es (Program Zero)
flatProgram Program {definitions} = Program <$> traverse flatDefinition definitions

flatDefinition :: (State Uniq :> es, Reader ModuleName :> es) => (t1, t2, t3, Statement One) -> Eff es (t1, t2, t3, Statement Zero)
flatDefinition (range, name, return, statement) = (range,name,return,) <$> flatStatement statement

-- | Wip is a temporary data structure to represent the intermediate state of the flattening process.
data Wip
  = Do' Range Name (Statement Zero)
  | Zero (Producer Zero)

cut :: Wip -> Consumer Zero -> Statement Zero
cut (Do' range name statement) consumer = CutDo range name statement consumer
cut (Zero producer) consumer = Cut producer consumer

flatStatement :: (State Uniq :> es, Reader ModuleName :> es) => Statement One -> Eff es (Statement Zero)
flatStatement (Cut producer consumer) = do
  producer <- flatProducer producer
  consumer <- flatConsumer consumer
  case producer of
    Do' range name statement -> pure $ CutDo range name statement consumer
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
        $ Then range var
        $ primitive
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

flatProducer :: (State Uniq :> es, Reader ModuleName :> es) => Producer One -> Eff es Wip
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

flatConsumer :: (State Uniq :> es, Reader ModuleName :> es) => Consumer One -> Eff es (Consumer Zero)
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
        $ cut (Zero (Var range outer))
        $ apply
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

flatBranch :: (State Uniq :> es, Reader ModuleName :> es) => Branch One -> Eff es (Branch Zero)
flatBranch (Branch range pattern statement) = do
  statement <- flatStatement statement
  pure $ Branch range pattern statement

split :: (State Uniq :> es, Reader ModuleName :> es) => [Producer One] -> Eff es ([Producer One], Maybe (Producer One), [Producer One])
split producers = aux [] producers
  where
    aux :: (State Uniq :> es, Reader ModuleName :> es) => [Producer One] -> [Producer One] -> Eff es ([Producer One], Maybe (Producer One), [Producer One])
    aux acc [] = pure (reverse acc, Nothing, [])
    aux acc (p : ps) = do
      p' <- flatProducer p
      case p' of
        Do' {} -> pure (reverse acc, Just p, ps)
        Zero {} -> aux (p : acc) ps

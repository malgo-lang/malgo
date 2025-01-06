{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Malgo.Core
  ( Producer (..),
    Copattern (..),
    Literal (..),
    Consumer (..),
    Pattern (..),
    Statement (..),
    Definition (..),
    focusDefinition,
    focus,
  )
where

import Control.Lens (makeFieldsId)
import Data.SCargot.Repr.Basic qualified as S
import Effectful.Log (Log)
import Malgo.Lens
import Malgo.Location
import Malgo.Name
import Malgo.Prelude
import Malgo.Unique (UniqueGen)

-- | @Producer@ represents a term that produces values
data Producer
  = Var
      { location :: Location,
        name :: Name
      }
  | Literal
      { location :: Location,
        literal :: Literal
      }
  | Do {location :: Location, name :: Name, statement :: Statement}
  | Construct
      { location :: Location,
        tag :: Text,
        producers :: [Producer],
        consumers :: [Consumer]
      }
  | Comatch
      { location :: Location,
        clauses :: [(Copattern, Statement)]
      }
  deriving (Show, Eq)

instance ToSExpr Producer where
  toSExpr Var {..} = toSExpr name
  toSExpr Literal {..} = toSExpr literal
  toSExpr Do {..} = S.L [S.A (Symbol "do"), toSExpr name, toSExpr statement]
  toSExpr Construct {..} = S.L [S.A (Symbol "$"), S.A (Symbol tag), S.L (toSExpr <$> producers), S.L (toSExpr <$> consumers)]
  toSExpr Comatch {..} =
    S.L
      $ [S.A (Symbol "comatch")]
      <> map (\(copattern, statement) -> S.L [toSExpr copattern, toSExpr statement]) clauses

data Copattern = Copattern
  { tag :: Text,
    params :: [Name],
    returns :: [Name]
  }
  deriving (Show, Eq)

instance ToSExpr Copattern where
  toSExpr Copattern {..} = S.L [S.A (Symbol tag), S.L (toSExpr <$> params), S.L (toSExpr <$> returns)]

-- | @Const@ represents a constant value
data Literal = Int {int :: Int}
  deriving (Show, Eq)

instance ToSExpr Literal where
  toSExpr Int {..} = S.A (Number int)

-- | @Consumer@ represents a term that consumes values
data Consumer
  = Finish
      { location :: Location
      }
  | Covar
      { location :: Location,
        name :: Name
      }
  | Then
      { location :: Location,
        name :: Name,
        statement :: Statement
      }
  | Destruct
      { location :: Location,
        tag :: Text,
        producers :: [Producer],
        consumers :: [Consumer]
      }
  | Match
      { location :: Location,
        clauses :: [(Pattern, Statement)]
      }
  deriving (Show, Eq)

instance ToSExpr Consumer where
  toSExpr Finish {} = S.A (Symbol "finish")
  toSExpr Covar {..} = toSExpr name
  toSExpr Then {..} = S.L [S.A (Symbol "then"), toSExpr name, toSExpr statement]
  toSExpr Destruct {..} = S.L [S.A (Symbol "."), S.A (Symbol tag), S.L (toSExpr <$> producers), S.L (toSExpr <$> consumers)]
  toSExpr Match {..} =
    S.L
      $ [S.A (Symbol "match")]
      <> map (\(pattern, statement) -> S.L [toSExpr pattern, toSExpr statement]) clauses

data Pattern = Pattern
  { tag :: Text,
    params :: [Name],
    returns :: [Name]
  }
  deriving (Show, Eq)

instance ToSExpr Pattern where
  toSExpr Pattern {..} = S.L [S.A (Symbol tag), S.L (toSExpr <$> params), S.L (toSExpr <$> returns)]

-- | @Statement@ represents a statement
data Statement
  = Prim
      { location :: Location,
        tag :: Text,
        producers :: [Producer],
        consumers :: [Consumer]
      }
  | Switch
      { location :: Location,
        producer :: Producer,
        clauses :: [(Literal, Statement)],
        statement :: Statement
      }
  | Cut
      { location :: Location,
        producer :: Producer,
        consumer :: Consumer
      }
  | Invoke
      { location :: Location,
        name :: Name,
        producers :: [Producer],
        consumers :: [Consumer]
      }
  deriving (Show, Eq)

instance ToSExpr Statement where
  toSExpr Prim {..} = S.L [S.A (Symbol "prim"), S.A (Symbol tag), S.L (toSExpr <$> producers), S.L (toSExpr <$> consumers)]
  toSExpr Switch {..} =
    S.L
      $ [S.A (Symbol "switch"), toSExpr producer]
      <> (map (\(literal, statement) -> S.L [toSExpr literal, toSExpr statement]) clauses)
      <> [S.L [S.A (Symbol "default"), toSExpr statement]]
  toSExpr Cut {..} = S.L [S.A (Symbol "cut"), toSExpr producer, toSExpr consumer]
  toSExpr Invoke {..} = S.L [S.A (Symbol "invoke"), toSExpr name, S.L (toSExpr <$> producers), S.L (toSExpr <$> consumers)]

-- | @Definition@ represents a top-level definition
data Definition = Definition
  { name :: Name,
    params :: [Name],
    returns :: [Name],
    statement :: Statement
  }
  deriving (Show, Eq)

instance ToSExpr Definition where
  toSExpr Definition {..} = S.L [S.A (Symbol "def"), toSExpr name, S.L (toSExpr <$> params), S.L (toSExpr <$> returns), toSExpr statement]

makeFieldsId ''Producer
makeFieldsId ''Consumer
makeFieldsId ''Statement
makeFieldsId ''Definition
makeFieldsId ''Copattern
makeFieldsId ''Pattern

focusDefinition :: (UniqueGen :> es, Log :> es) => Definition -> Eff es Definition
focusDefinition Definition {..} =
  Definition name params returns <$> focus statement

focus :: (Log :> es, UniqueGen :> es) => Statement -> Eff es Statement
focus Cut {..} = do
  Cut location <$> focusProducer producer <*> focusConsumer consumer
focus Switch {..}
  | isAtom producer = do
      Switch location
        <$> focusProducer producer
        <*> traverse (\(literal, statement) -> (literal,) <$> focus statement) clauses
        <*> focus statement
  | otherwise = do
      bind <- newName "switch_focus"
      Cut location
        <$> focusProducer producer
        <*> ( Then location bind
                <$> focus
                  ( Switch
                      location
                      (Var location bind)
                      clauses
                      statement
                  )
            )
focus Prim {..}
  | all isAtom producers =
      Prim location tag <$> traverse focusProducer producers <*> traverse focusConsumer consumers
  | otherwise = do
      let (values, mid, rest) = partitionAtoms producers
      bind <- newName "prim_focus"
      Cut location
        <$> focusProducer mid
        <*> ( Then location bind
                <$> focus
                  ( Prim
                      location
                      tag
                      (values <> [Var location bind] <> rest)
                      consumers
                  )
            )
focus Invoke {..}
  | all isAtom producers = do
      Invoke location name <$> traverse focusProducer producers <*> traverse focusConsumer consumers
  | otherwise = do
      let (values, mid, rest) = partitionAtoms producers
      bind <- newName "invoke_focus"
      Cut location
        <$> focusProducer mid
        <*> ( Then location bind
                <$> focus
                  ( Invoke
                      location
                      name
                      (values <> [Var location bind] <> rest)
                      consumers
                  )
            )

isAtom :: Producer -> Bool
isAtom Do {} = False
isAtom _ = True

partitionAtoms :: [Producer] -> ([Producer], Producer, [Producer])
partitionAtoms = go []
  where
    go _ [] = error "splitAtAtoms: empty list"
    go acc (x : xs)
      | isAtom x = go (x : acc) xs
      | otherwise = (reverse acc, x, xs)

focusProducer :: (UniqueGen :> es, Log :> es) => Producer -> Eff es Producer
focusProducer var@Var {} = pure var
focusProducer literal@Literal {} = pure literal
focusProducer Do {..} =
  Do location name <$> focus statement
focusProducer Construct {..}
  | all isAtom producers = do
      Construct location tag <$> traverse focusProducer producers <*> traverse focusConsumer consumers
  | otherwise = do
      bindConsumer <- newName "construct_label"
      bindProducer <- newName "construct_focus"
      let (values, mid, rest) = partitionAtoms producers
      mid' <- focusProducer mid
      producer <-
        focusProducer
          ( Construct
              location
              tag
              (values <> [Var location bindProducer] <> rest)
              consumers
          )
      pure
        $ Do location bindConsumer
        $ Cut location mid'
        $ Then location bindProducer
        $ Cut location producer (Covar location bindConsumer)
focusProducer Comatch {..} = do
  Comatch location
    <$> traverse (\(copattern, statement) -> (copattern,) <$> focus statement) clauses

focusConsumer :: (UniqueGen :> es, Log :> es) => Consumer -> Eff es Consumer
focusConsumer Finish {..} = pure $ Finish location
focusConsumer Covar {..} = pure $ Covar location name
focusConsumer Then {..} = Then location name <$> focus statement
focusConsumer Destruct {..}
  | all isAtom producers = do
      Destruct location tag
        <$> traverse focusProducer producers
        <*> traverse focusConsumer consumers
  | otherwise = do
      bindOuter <- newName "destruct_bind"
      bindInner <- newName "destruct_focus"
      let (values, mid, rest) = partitionAtoms producers
      mid' <- focusProducer mid
      statement <-
        focusConsumer
          ( Destruct
              location
              tag
              (values <> [Var location bindInner] <> rest)
              consumers
          )
      pure
        $ Then location bindOuter
        $ Cut location mid'
        $ Then location bindInner
        $ Cut location (Var location bindOuter) statement
focusConsumer Match {..} = do
  Match location
    <$> traverse (\(pattern, body) -> (pattern,) <$> focus body) clauses
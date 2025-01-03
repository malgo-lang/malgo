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

data Copattern = Copattern
  { tag :: Text,
    params :: [Name],
    returns :: [Name]
  }
  deriving (Show, Eq)

-- | @Const@ represents a constant value
data Literal = Int {int :: Int}
  deriving (Show, Eq)

-- | @Consumer@ represents a term that consumes values
data Consumer
  = Finish
      { location :: Location
      }
  | Label
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

data Pattern = Pattern
  { tag :: Text,
    params :: [Name],
    returns :: [Name]
  }
  deriving (Show, Eq)

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

-- | @Definition@ represents a top-level definition
data Definition = Definition
  { name :: Name,
    params :: [Name],
    returns :: [Name],
    statement :: Statement
  }
  deriving (Show, Eq)

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
        $ Cut location producer (Label location bindConsumer)
focusProducer Comatch {..} = do
  Comatch location
    <$> traverse (\(copattern, statement) -> (copattern,) <$> focus statement) clauses

focusConsumer :: (UniqueGen :> es, Log :> es) => Consumer -> Eff es Consumer
focusConsumer Finish {..} = pure $ Finish location
focusConsumer Label {..} = pure $ Label location name
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
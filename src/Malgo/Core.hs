{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Malgo.Core
  ( Producer (..),
    Copattern,
    Literal (..),
    Consumer (..),
    Pattern,
    Statement (..),
    Definition (..),
    focus,
    focusDefinition,
  )
where

import Data.Traversable (for)
import Effectful.Log (Log)
import Malgo.Location
import Malgo.Name
import Malgo.Prelude
import Malgo.Unique (UniqueGen)

-- | @Producer@ represents a term that produces values
data Producer
  = Var Location Name
  | Literal Location Literal
  | Do Location Name Statement
  | Construct Location Text [Producer] [Consumer]
  | Comatch Location [(Copattern, Statement)]
  deriving (Show, Eq)

instance HasLocation Producer where
  location (Var loc _) = loc
  location (Literal loc _) = loc
  location (Do loc _ _) = loc
  location (Construct loc _ _ _) = loc
  location (Comatch loc _) = loc

type Copattern = (Text, [Name], [Name])

-- | @Const@ represents a constant value
data Literal = Int Int
  deriving (Show, Eq)

-- | @Consumer@ represents a term that consumes values
data Consumer
  = Finish Location
  | Label Location Name
  | Then Location Name Statement
  | Destruct Location Text [Producer] [Consumer]
  | Match Location [(Pattern, Statement)]
  deriving (Show, Eq)

instance HasLocation Consumer where
  location (Finish loc) = loc
  location (Label loc _) = loc
  location (Then loc _ _) = loc
  location (Destruct loc _ _ _) = loc
  location (Match loc _) = loc

type Pattern = (Text, [Name], [Name])

-- | @Statement@ represents a statement
data Statement
  = Prim Location Text [Producer] Consumer
  | Switch Location Producer [(Literal, Statement)] Statement
  | Cut Location Producer Consumer
  | Invoke Location Name [Producer] [Consumer]
  deriving (Show, Eq)

instance HasLocation Statement where
  location (Prim loc _ _ _) = loc
  location (Switch loc _ _ _) = loc
  location (Cut loc _ _) = loc
  location (Invoke loc _ _ _) = loc

-- | @Definition@ represents a top-level definition
data Definition = Definition
  { name :: Name,
    params :: [Name],
    returns :: [Name],
    body :: Statement
  }
  deriving (Show, Eq)

focusDefinition :: (UniqueGen :> es, Log :> es) => Definition -> Eff es Definition
focusDefinition (Definition name params returns body) =
  Definition name params returns <$> focus body

focus :: (Log :> es, UniqueGen :> es) => Statement -> Eff es Statement
focus (Cut loc prod cons) = do
  prod' <- focusProducer prod
  cons' <- focusConsumer cons
  pure $ Cut loc prod' cons'
focus (Switch loc prod branches defBranch)
  | isAtom prod = do
      prod' <- focusProducer prod
      branches' <- for branches \(lit, body) -> do
        body' <- focus body
        pure (lit, body')
      def' <- focus defBranch
      pure $ Switch loc prod' branches' def'
  | otherwise = do
      prod' <- focusProducer prod
      val <- newName "switch_focus"
      body <- focus (Switch loc (Var loc val) branches defBranch)
      pure $ Cut loc prod' (Then loc val body)
focus (Prim loc name args cons)
  | all isAtom args =
      Prim loc name <$> traverse focusProducer args <*> focusConsumer cons
  | otherwise = do
      let (values, mid, rest) = partitionAtoms args
      mid' <- focusProducer mid
      val <- newName "prim_focus"
      body <- focus (Prim loc name (values <> [Var loc val] <> rest) cons)
      pure $ Cut loc mid' (Then loc val body)
focus (Invoke loc name args cons)
  | all isAtom args = do
      Invoke loc name <$> traverse focusProducer args <*> traverse focusConsumer cons
  | otherwise = do
      let (values, mid, rest) = partitionAtoms args
      mid' <- focusProducer mid
      x <- newName "invoke_focus"
      body <- focus (Invoke loc name (values <> [Var loc x] <> rest) cons)
      pure $ Cut loc mid' (Then loc x body)

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
focusProducer (Var loc name) = pure $ Var loc name
focusProducer (Literal loc lit) = pure $ Literal loc lit
focusProducer (Do loc name body) = do
  Do loc name <$> focus body
focusProducer (Construct loc name args conts)
  | all isAtom args = do
      Construct loc name <$> traverse focusProducer args <*> traverse focusConsumer conts
  | otherwise = do
      a <- newName "construct_label"
      let (values, mid, rest) = partitionAtoms args
      mid' <- focusProducer mid
      x <- newName "construct_focus"
      body <- focusProducer (Construct loc name (values <> [Var loc x] <> rest) conts)
      pure
        $ Do loc a
        $ Cut loc mid'
        $ Then loc x
        $ Cut loc body (Label loc a)
focusProducer (Comatch loc branches) = do
  branches' <- for branches \(copat, body) -> do
    body' <- focus body
    pure (copat, body')
  pure $ Comatch loc branches'

focusConsumer :: (UniqueGen :> es, Log :> es) => Consumer -> Eff es Consumer
focusConsumer (Finish loc) = pure $ Finish loc
focusConsumer (Label loc name) = pure $ Label loc name
focusConsumer (Then loc name body) = Then loc name <$> focus body
focusConsumer (Destruct loc name args conts)
  | all isAtom args = do
      Destruct loc name <$> traverse focusProducer args <*> traverse focusConsumer conts
  | otherwise = do
      y <- newName "destruct_bind"
      let (values, mid, rest) = partitionAtoms args
      mid' <- focusProducer mid
      x <- newName "destruct_focus"
      body <- focusConsumer (Destruct loc name (values <> [Var loc x] <> rest) conts)
      pure
        $ Then loc y
        $ Cut loc mid'
        $ Then loc x
        $ Cut loc (Var loc y) body
focusConsumer (Match loc branches) = do
  branches' <- for branches \(pat, body) -> do
    body' <- focus body
    pure (pat, body')
  pure $ Match loc branches'

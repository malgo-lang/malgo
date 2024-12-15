{-# LANGUAGE TypeFamilies #-}

module Malgo.Core
  ( Producer (..),
    Copattern,
    Literal (..),
    Consumer (..),
    Pattern,
    Statement (..),
    Definition (..),
  )
where

import Malgo.Location
import Malgo.Name
import Malgo.Prelude

-- | @Producer@ represents a term that produces values
data Producer
  = Var Location Name
  | Literal Location Literal
  | Do Location Name Statement
  | Construct Location Name [Producer] [Consumer]
  | Cocase Location [(Copattern, Statement)]
  deriving (Show, Eq)

instance HasLocation Producer where
  location (Var loc _) = loc
  location (Literal loc _) = loc
  location (Do loc _ _) = loc
  location (Construct loc _ _ _) = loc
  location (Cocase loc _) = loc

type Copattern = (Name, [Name], [Name])

-- | @Const@ represents a constant value
data Literal = Int Int
  deriving (Show, Eq)

-- | @Consumer@ represents a term that consumes values
data Consumer
  = Finish Location
  | Label Location Name
  | Then Location Name Statement
  | Destruct Location Name [Producer] [Consumer]
  | Case Location [(Pattern, Statement)]
  deriving (Show, Eq)

instance HasLocation Consumer where
  location (Finish loc) = loc
  location (Label loc _) = loc
  location (Then loc _ _) = loc
  location (Destruct loc _ _ _) = loc
  location (Case loc _) = loc

type Pattern = (Name, [Name], [Name])

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
  deriving (Show)
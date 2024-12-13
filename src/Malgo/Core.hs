{-# LANGUAGE TypeFamilies #-}

module Malgo.Core
  ( Location (..),
    fromCallStack,
    HasLocation (..),
    UniqueGen,
    newUnique,
    Name (..),
    newName,
    Producer (..),
    Const (..),
    Consumer (..),
    Statement (..),
  )
where

import Effectful.State.Static.Shared qualified as Shared
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Malgo.Prelude

-- | @Location@ represents a source file location
data Location = Location
  { fileName :: FilePath,
    line :: Int,
    column :: Int
  }
  deriving (Eq, Ord)

instance Show Location where
  show (Location file l c) = file ++ ":" ++ show l ++ ":" ++ show c

instance Read Location where
  readsPrec _ s =
    case break (== ':') s of
      (file, _ : rest) ->
        case break (== ':') rest of
          (l, _ : c) -> [(Location file (read l) (read c), "")]
          _ -> []
      _ -> []

-- | @fromCallStack@ converts a @CallStack@ to a @Location@
fromCallStack :: (HasCallStack) => Location
fromCallStack =
  case getCallStack callStack of
    (_, loc) : _ ->
      Location
        { fileName = srcLocFile loc,
          line = srcLocStartLine loc,
          column = srcLocStartCol loc
        }
    _ -> error "fromCallStack: empty call stack"

-- | @UniqueGen@ is an effect for generating unique numbers
type UniqueGen = Shared.State Int

-- | @newUnique@ generates a new unique number
newUnique :: (UniqueGen :> es) => Eff es Int
newUnique = do
  i <- Shared.get
  Shared.put (i + 1)
  pure i

-- | @Name@ represents an identifier
data Name = Name
  { text :: Text,
    unique :: Int
  }
  deriving (Eq, Ord)

instance Show Name where
  show (Name t u) = convertString t ++ "." ++ show u

instance Read Name where
  readsPrec _ s =
    case break (== '.') s of
      (t, _ : rest) ->
        case reads rest of
          [(u, "")] -> [(Name (convertString t) u, "")]
          _ -> []
      _ -> []

-- | @newName@ creates a new name with a given text
newName :: (UniqueGen :> es) => Text -> Eff es Name
newName t = do
  u <- newUnique
  pure (Name t u)

-- | @HasLocation@ is a type class for types that have a location
class HasLocation a where
  location :: a -> Location

instance HasLocation Location where
  location = id

-- | @Producer@ represents a term that produces values
data Producer
  = Var Location Name
  | Const Location Const
  | Do Location Name Statement
  deriving (Show)

instance HasLocation Producer where
  location (Var loc _) = loc
  location (Const loc _) = loc
  location (Do loc _ _) = loc

-- | @Const@ represents a constant value
data Const = Int Int
  deriving (Show)

-- | @Consumer@ represents a term that consumes values
data Consumer
  = Finish Location
  | Label Location Name
  | Then Location Name Statement
  deriving (Show)

instance HasLocation Consumer where
  location (Finish loc) = loc
  location (Label loc _) = loc
  location (Then loc _ _) = loc

-- | @Statement@ represents a statement
data Statement
  = Prim Location Text [Producer] Consumer
  | Switch Location Producer [(Const, Statement)] Statement
  | Cut Location Producer Consumer
  | Invoke Location Name [Producer] [Consumer]
  deriving (Show)

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
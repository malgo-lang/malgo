{-# LANGUAGE TypeFamilies #-}

module Malgo.Name
  ( Name (..),
    newName,
    HasName (..),
  )
where

import Control.Lens (Traversal')
import Malgo.Prelude
import Malgo.Unique
import Text.Show qualified as Show

-- | @Name@ represents an identifier
data Name = Name
  { text :: Text,
    unique :: Int
  }
  deriving (Eq, Ord)

instance Show Name where
  show (Name t u) = convertString t ++ "_" ++ show u

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

class HasName s a | s -> a where
  name :: Traversal' s a

instance HasName Name Name where
  name = id

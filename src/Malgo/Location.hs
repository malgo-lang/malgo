{-# LANGUAGE TypeFamilies #-}

module Malgo.Location
  ( Location (..),
    fromCallStack,
    HasLocation (..),
  )
where

import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Malgo.Prelude
import Text.Show qualified as Show

-- | @Location@ represents a source file location
data Location = Location
  { fileName :: FilePath,
    line :: Int,
    column :: Int
  }
  deriving (Eq, Ord)

instance Show Location where
  show (Location file l c) = file ++ ":" <> show l <> ":" ++ show c

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

-- | @HasLocation@ is a type class for types that have a location
class HasLocation a where
  location :: a -> Location

instance HasLocation Location where
  location = id
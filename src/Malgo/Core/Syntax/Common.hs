module Malgo.Core.Syntax.Common
  ( HasFreeVar (..),
    toJSONTagged,
    parseJSONTagged,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.List qualified as List
import Malgo.Prelude

-- | 'f' may have free variables
-- 'freevars' does not include callees of `call-direct`.
-- If you want to include callees of `call-direct`, merge 'callees' and 'freevars'.
class HasFreeVar f where
  -- | Free variables.
  -- It does not include callees of `call-direct`.
  freevars :: (Ord a) => f a -> Set a

  -- | Callees.
  callees :: (Ord a) => f a -> Set a

toJSONTagged :: Text -> [Pair] -> Value
toJSONTagged tag pairs = object $ "tag" .= tag : pairs

parseJSONTagged :: String -> [(String, Object -> Parser a)] -> Value -> Parser a
parseJSONTagged name alts = withObject name \v -> do
  tag <- v .: "tag"
  case List.lookup tag alts of
    Just f -> f v
    Nothing -> fail $ "unknown tag: " <> tag

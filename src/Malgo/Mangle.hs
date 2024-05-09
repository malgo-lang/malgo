module Malgo.Mangle (Manglable (..), mangle, demangle) where

import Control.Exception (assert)
import Data.Text qualified as T
import Data.Text.ICU.Char (Bool_ (IDContinue), property)
import Malgo.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)

class Manglable a where
  toTextList :: a -> [Text]
  fromTextList :: [Text] -> a

mangle :: (Manglable a) => a -> Text
mangle = mangleTextList . toTextList

demangle :: (Manglable a) => Text -> a
demangle = fromTextList . demangleTextList

{-
Note for mangling identifiers:

Ideas

How to mangle identifiers:
- Replace all '_' with _x5F
- Replace all the characters except for ID_Continue with _xXX, _uXXXX, or _UXXXXXXXX.
- Concatenate the names with the run-length encoding.
- Add the prefix "_M".

How to demangle identifiers:
- Remove the prefix "_M".
- Split the names with the run-length encoding if necessary.
- Replace all the _xXX, _uXXXX, and _UXXXXXXXX with the corresponding characters.
-}

-- | Mangle a list of identifiers.
--
-- >>> mangleTextList ["foo", "bar"]
-- "_M3foo3bar"
-- >>> mangleTextList ["foo/bar.mlg", "baz"]
-- "_M17foo_x2Fbar_x2Emlg3baz"
-- >>> mangleTextList ["foo/bar.mlg", "baz_42"]
-- "_M17foo_x2Fbar_x2Emlg9baz_x5F42"
-- >>> mangleTextList ["+"]
-- "_M4_x2B"
-- >>> mangleTextList ["->"]
-- "_M8_x2D_x3E"
-- >>> mangleTextList ["あいうえお"]
-- "_M5\12354\12356\12358\12360\12362"
-- >>> mangleTextList ["→"]
-- "_M6_u2192"
mangleTextList :: [Text] -> Text
mangleTextList msgs =
  let underscoreReplaced = map (T.concatMap replaceUnderscore) msgs
      otherReplaced = map (T.concatMap replaceOther) underscoreReplaced
      runLengthed = map runLength otherReplaced
   in "_M" <> T.concat runLengthed
  where
    replaceUnderscore '_' = "_x5F"
    replaceUnderscore c = T.singleton c
    replaceOther c
      | property IDContinue c = T.singleton c
      | ord c <= 0xFF =
          T.pack
            [ '_',
              'x',
              intToHexNth 1 c,
              intToHexNth 0 c
            ]
      | ord c <= 0xFFFF =
          T.pack
            [ '_',
              'u',
              intToHexNth 3 c,
              intToHexNth 2 c,
              intToHexNth 1 c,
              intToHexNth 0 c
            ]
      | otherwise =
          T.pack
            [ '_',
              'U',
              intToHexNth 7 c,
              intToHexNth 6 c,
              intToHexNth 5 c,
              intToHexNth 4 c,
              intToHexNth 3 c,
              intToHexNth 2 c,
              intToHexNth 1 c,
              intToHexNth 0 c
            ]
    intToHexNth n c = intToHex (ord c `div` power n `mod` 16)
    intToHex n
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'A' + n - 10)
    power n = 16 ^ (n :: Int)
    runLength x
      | T.null x = ""
      | otherwise = assert (not $ isDigit $ T.head x) $ T.pack (show $ T.length x) <> x

-- | DemangleTextList a mangleTextListd identifier.
--
-- >>> demangleTextList "_M3foo3bar"
-- ["foo","bar"]
-- >>> demangleTextList "_M17foo_x2Fbar_x2Emlg3baz"
-- ["foo/bar.mlg","baz"]
-- >>> demangleTextList "_M17foo_x2Fbar_x2Emlg9baz_x5F42"
-- ["foo/bar.mlg","baz_42"]
-- >>> demangleTextList "_M4_x2B"
-- ["+"]
-- >>> demangleTextList "_M8_x2D_x3E"
-- ["->"]
-- >>> demangleTextList "_M5あいうえお"
-- ["\12354\12356\12358\12360\12362"]
-- >>> demangleTextList "_M6_u2192"
-- ["\8594"]
--
-- prop> \x -> any null x || any (isDigit . head) x || demangleTextList (mangleTextList (map T.pack x)) == map T.pack x
demangleTextList :: (HasCallStack) => Text -> [Text]
demangleTextList msg
  | "_M" `T.isPrefixOf` msg =
      let prefixRemoved = T.drop 2 msg
          msgs = splitByRunLength prefixRemoved
       in map replace msgs
  | otherwise = error "demangleTextList: not a mangleTextListd identifier"
  where
    splitByRunLength msg =
      case Text.Megaparsec.runParser
        ( many (takeP Nothing . read =<< some digitChar) ::
            Parsec Void Text [Text]
        )
        "<demangleTextList>"
        msg of
        Left e -> error $ errorBundlePretty e
        Right x -> x
    replace txt = case T.splitOn "_" txt of
      [] -> error "demangleTextList: empty string"
      [x] -> x
      (x : xs) -> mconcat $ x : map replace' xs
    replace' txt
      | "x" `T.isPrefixOf` txt =
          case T.unpack $ T.drop 1 txt of
            (a : b : rest) ->
              T.cons
                ( chr
                    $ hexToIntNth 1 a
                    + hexToIntNth 0 b
                )
                (T.pack rest)
            _ -> error $ "demangleTextList: invalid hex " <> convertString txt
      | "u" `T.isPrefixOf` txt =
          case T.unpack $ T.drop 1 txt of
            (a : b : c : d : rest) ->
              T.cons
                ( chr
                    $ hexToIntNth 3 a
                    + hexToIntNth 2 b
                    + hexToIntNth 1 c
                    + hexToIntNth 0 d
                )
                (T.pack rest)
            _ -> error $ "demangleTextList: invalid hex " <> convertString txt
      | "U" `T.isPrefixOf` txt =
          case T.unpack $ T.drop 1 txt of
            (a : b : c : d : e : f : g : h : rest) ->
              T.cons
                ( chr
                    $ hexToIntNth 7 a
                    + hexToIntNth 6 b
                    + hexToIntNth 5 c
                    + hexToIntNth 4 d
                    + hexToIntNth 3 e
                    + hexToIntNth 2 f
                    + hexToIntNth 1 g
                    + hexToIntNth 0 h
                )
                (T.pack rest)
            _ -> error $ "demangleTextList: invalid hex " <> convertString txt
      | otherwise = error "demangleTextList: invalid prefix"
    hexToIntNth n c = hexToInt c * power n
    power n = 16 ^ (n :: Int)
    hexToInt c
      | isDigit c = ord c - ord '0'
      | 'A' <= c && c <= 'F' = ord c - ord 'A' + 10
      | 'a' <= c && c <= 'f' = ord c - ord 'a' + 10
      | otherwise = error $ "demangleTextList: invalid hex " <> [c]

module Malgo.Mangle (mangle, demangle) where

import Control.Exception (assert)
import Data.Text qualified as T
import Data.Text.ICU.Char (Bool_ (IDContinue), property)
import Malgo.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)

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
-- >>> mangle ["foo", "bar"]
-- "_M3foo3bar"
-- >>> mangle ["foo/bar.mlg", "baz"]
-- "_M17foo_x2Fbar_x2Emlg3baz"
-- >>> mangle ["foo/bar.mlg", "baz_42"]
-- "_M17foo_x2Fbar_x2Emlg9baz_x5F42"
-- >>> mangle ["+"]
-- "_M4_x2B"
-- >>> mangle ["->"]
-- "_M8_x2D_x3E"
-- >>> mangle ["あいうえお"]
-- "_M5\12354\12356\12358\12360\12362"
mangle :: [Text] -> Text
mangle msgs =
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
              intToHex (ord c `div` power 1),
              intToHex (ord c `mod` 16)
            ]
      | ord c <= 0xFFFF =
          T.pack
            [ '_',
              'u',
              intToHex (ord c `div` power 3),
              intToHex (ord c `div` power 2 `mod` 16),
              intToHex (ord c `div` power 1 `mod` 16),
              intToHex (ord c `mod` 16)
            ]
      | otherwise =
          T.pack
            [ '_',
              'U',
              intToHex (ord c `div` power 7),
              intToHex (ord c `div` power 6 `mod` 16),
              intToHex (ord c `div` power 5 `mod` 16),
              intToHex (ord c `div` power 4 `mod` 16),
              intToHex (ord c `div` power 3 `mod` 16),
              intToHex (ord c `div` power 2 `mod` 16),
              intToHex (ord c `div` power 1 `mod` 16),
              intToHex (ord c `mod` 16)
            ]
    intToHex n
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'A' + n - 10)
    power n = 16 ^ (n :: Int)
    runLength x
      | T.null x = ""
      | otherwise = assert (not $ isDigit $ T.head x) $ T.pack (show $ T.length x) <> x

-- | Demangle a mangled identifier.
--
-- >>> demangle "_M3foo3bar"
-- ["foo","bar"]
-- >>> demangle "_M17foo_x2Fbar_x2Emlg3baz"
-- ["foo/bar.mlg","baz"]
-- >>> demangle "_M17foo_x2Fbar_x2Emlg9baz_x5F42"
-- ["foo/bar.mlg","baz_42"]
-- >>> demangle "_M4_x2B"
-- ["+"]
-- >>> demangle "_M8_x2D_x3E"
-- ["->"]
-- >>> demangle "_M5あいうえお"
-- ["\12354\12356\12358\12360\12362"]
--
-- prop> \x -> any null x || any (isDigit . head) x || demangle (mangle (map T.pack x)) == map T.pack x
demangle :: (HasCallStack) => Text -> [Text]
demangle msg
  | "_M" `T.isPrefixOf` msg =
      let prefixRemoved = T.drop 2 msg
          msgs = splitByRunLength prefixRemoved
       in map replace msgs
  | otherwise = error "demangle: not a mangled identifier"
  where
    splitByRunLength msg =
      case Text.Megaparsec.runParser (many pSingle) "<demangle>" msg of
        Left e -> error $ errorBundlePretty e
        Right x -> x
    pSingle :: Parsec Void Text Text
    pSingle = do
      n <- read <$> some digitChar
      takeP Nothing n
    replace txt = case T.splitOn "_" txt of
      [] -> error "demangle: empty string"
      [x] -> x
      (x : xs) -> mconcat $ x : map replace' xs
    replace' txt
      | "x" `T.isPrefixOf` txt =
          case T.unpack $ T.drop 1 txt of
            (a : b : rest) -> T.cons (chr $ hexToInt a * power 1 + hexToInt b) (T.pack rest)
            _ -> error $ "demangle: invalid hex " <> convertString txt
      | "u" `T.isPrefixOf` txt =
          case T.unpack $ T.drop 1 txt of
            (a : b : c : d : rest) ->
              T.cons
                ( chr
                    $ hexToInt a
                    * power 3
                    + hexToInt b
                    * power 2
                    + hexToInt c
                    * power 1
                    + hexToInt d
                )
                (T.pack rest)
            _ -> error $ "demangle: invalid hex " <> convertString txt
      | "U" `T.isPrefixOf` txt =
          case T.unpack $ T.drop 1 txt of
            (a : b : c : d : e : f : g : h : rest) ->
              T.cons
                ( chr
                    $ hexToInt a
                    * power 7
                    + hexToInt b
                    * power 6
                    + hexToInt c
                    * power 5
                    + hexToInt d
                    * power 4
                    + hexToInt e
                    * power 3
                    + hexToInt f
                    * power 2
                    + hexToInt g
                    * power 1
                    + hexToInt h
                )
                (T.pack rest)
            _ -> error $ "demangle: invalid hex " <> convertString txt
      | otherwise = error "demangle: invalid prefix"
    power n = 16 ^ (n :: Int)
    hexToInt c
      | isDigit c = ord c - ord '0'
      | 'A' <= c && c <= 'F' = ord c - ord 'A' + 10
      | 'a' <= c && c <= 'f' = ord c - ord 'a' + 10
      | otherwise = error "demangle: invalid hex"

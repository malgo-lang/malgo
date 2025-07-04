module Malgo.Parser.Lexer
  ( -- * Basic combinators
    space,
    lexeme,
    symbol,

    -- * Identifiers and operators
    ident,
    operator,
    reserved,
    reservedOperator,

    -- * Literals
    decimal,
    pStringLiteral,

    -- * Pragma handling
    extractPragmas,
    skipPragma,
  )
where

import Data.Text.Lazy qualified as TL
import Malgo.Parser.Common
import Malgo.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

space :: Parser es ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser es a -> Parser es a
lexeme = L.lexeme space

symbol :: TL.Text -> Parser es ()
symbol = void . L.symbol space

decimal :: Parser es Int
decimal = lexeme L.decimal

ident :: Parser es Text
ident = lexeme do
  notFollowedBy anyReserved
  TL.toStrict . TL.pack <$> ((:) <$> identStart <*> many identContinue)

identStart :: Parser es Char
identStart = letterChar <|> char '_'

identContinue :: Parser es Char
identContinue = alphaNumChar <|> char '_' <|> char '#'

reserved :: TL.Text -> Parser es ()
reserved w
  | w `elem` reservedKeywords = void $ lexeme (string w <* notFollowedBy identContinue)
  | otherwise = fail $ "reserved keyword: " <> show w

anyReserved :: Parser es ()
anyReserved = choice $ map (try . reserved) reservedKeywords

reservedKeywords :: [TL.Text]
reservedKeywords =
  [ "class",
    "def",
    "data",
    "exists",
    "forall",
    "foreign",
    "impl",
    "import",
    "infix",
    "infixl",
    "infixr",
    "let",
    "type",
    "module",
    "with"
  ]

operator :: Parser es Text
operator = lexeme do
  notFollowedBy anyReservedOperator
  convertString <$> some operatorChar

operatorChar :: Parser es Char
operatorChar = oneOf ("+-*/\\%=><:;|&!#." :: String)

reservedOperator :: TL.Text -> Parser es ()
reservedOperator w
  | w `elem` reservedOperators = void $ lexeme (string w <* notFollowedBy operatorChar)
  | otherwise = fail $ "reserved symbol: " <> show w

anyReservedOperator :: Parser es ()
anyReservedOperator = choice $ map (try . reservedOperator) reservedOperators

reservedOperators :: [TL.Text]
reservedOperators = ["=>", "=", ":", "|", "->", ";", ".", ",", "!", "#|", "|#"]

pStringLiteral :: (ConvertibleStrings String a) => Parser es a
pStringLiteral = lexeme do
  void $ char '"'
  str <- manyTill L.charLiteral (char '"')
  pure $ convertString str

extractPragmas :: TL.Text -> [Text]
extractPragmas = go [] . TL.lines
  where
    go pragmas [] = map convertString $ reverse pragmas
    go pragmas (l : ls)
      | "#" `TL.isPrefixOf` l = go (TL.drop 1 l : pragmas) ls
      | otherwise = go pragmas ls

skipPragma :: Parser es ()
skipPragma = lexeme do
  void $ char '#'
  void $ takeWhileP (Just "pragma") (/= '\n')
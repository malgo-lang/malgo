{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Koriel.Core.Parser where

import Error.Diagnose.Compat.Megaparsec
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Koriel.Core.Syntax
import Koriel.Prelude hiding (many, some)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

parse :: String -> Text -> Either (ParseErrorBundle Text Void) (Program Text)
parse = Megaparsec.parse do
  space
  undefined

type Parser = Parsec Void Text

instance HasHints Void Text where
  hints = const []

-- | Parse an unboxed literal.
unboxed :: Parser Unboxed
unboxed = try int32 <|> try int64 <|> try float <|> double <|> char <|> string <|> bool
  where
    int32 = lexeme do
      xs <- Lexer.decimal
      void $ Char.string "_i32"
      pure $ Int32 xs
    int64 = lexeme do
      xs <- Lexer.decimal
      void $ Char.string "_i64"
      pure $ Int64 xs
    float = lexeme do
      xs <- castWord32ToFloat <$> Lexer.hexadecimal
      void $ Char.string "_f32"
      pure $ Float xs
    double = lexeme do
      xs <- castWord64ToDouble <$> Lexer.hexadecimal
      void $ Char.string "_f64"
      pure $ Double xs
    char = lexeme do
      Char <$> between (Char.char '\'') (Char.char '\'') Lexer.charLiteral
    string = lexeme do
      String . toText <$> (Char.char '"' *> Lexer.charLiteral `manyTill` Char.char '"')
    bool = lexeme do
      xs <- Char.string "True#" <|> Char.string "False#"
      pure $ Bool $ xs == "True#"

-- | Parse an atom.
atom :: Parser (Atom Text)
atom = try (Var <$> ident) <|> Unboxed <$> unboxed

-- | Parse an object.
object :: Parser (Obj Text)
object = between (symbol "(") (symbol ")") do
  fun <|> pack <|> record
  where
    fun = do
      void $ symbol "fun"
      xs <- between (symbol "(") (symbol ")") (many ident)
      Fun xs <$> expr
    pack = do
      undefined
    record = do
      undefined

-- | Parse an expression.
expr = undefined

-- * Common combinators

-- | Skip whitespace and comments.
space :: Parser ()
space = Lexer.space Char.space1 lineComment blockComment
  where
    lineComment = Lexer.skipLineComment ";"
    blockComment = Lexer.skipBlockCommentNested "#|" "|#"

-- | Apply a parser and skip trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

-- | Parse a symbol and skip trailing whitespace.
symbol :: Text -> Parser Text
symbol = Lexer.symbol space

-- | Character that can be used in an identifier.
-- Basically, it is the same as 'Malgo.Parser.identLetter', but we also allow '$' for temporary variables.
identLetter :: Parser Char
identLetter = Char.alphaNumChar <|> oneOf ("_+-*/\\%=><:;|&!#." :: String)

-- | Parse an identifier.
-- In Koriel, we always know where an identifier appears,
-- so we don't need to check if it is a keyword.
-- (And identifiers that textually look like keywords are allowed.)
ident :: Parser Text
ident = lexeme do
  xs <- some identLetter
  pure $ toText xs
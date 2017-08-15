module Language.Malgo.Parser
  where

import           Language.Malgo.Syntax

import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token     as Tok

lexer = Tok.makeTokenParser $ emptyDef {
    Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.identStart = letter <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.identLetter = alphaNum <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.reservedOpNames = [":"]
  , Tok.reservedNames = ["#t", "#f"]
  }

integer = Tok.integer lexer
float = Tok.float lexer
parens = Tok.parens lexer
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
brackets = Tok.brackets lexer
lexeme = Tok.lexeme lexer
stringLiteral = Tok.stringLiteral lexer
charLiteral = Tok.charLiteral lexer

parseUntyped = try (fmap Symbol identifier)
  <|> try (fmap Float float)
  <|> try (fmap Int integer)
  <|> try (reserved "#t" >> return (Bool True))
  <|> try (reserved "#f" >> return (Bool False))
  <|> try (fmap Char charLiteral)
  <|> try (fmap String stringLiteral)
  <|> try (char '\'' >> identifier >>= \s -> return (Tree [Symbol "quote", Symbol s]))
  <|> fmap List (brackets (many parseExpr))
  <|> fmap Tree (parens (many parseExpr))

parseTyped = parseUntyped >>= \e -> reservedOp ":" >> parseUntyped >>= \t -> return (Typed e t)

parseExpr = try parseTyped <|> parseUntyped

parse = Text.Parsec.parse parseExpr "test"

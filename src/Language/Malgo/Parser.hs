module Language.Malgo.Parser where

import           Language.Malgo.Syntax
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token     as Tok

lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = ";"
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
symbol = Tok.symbol lexer

{-- Syntax.hs
data AST = Symbol Name
         | Int Integer
         | Float Double
         | Bool Bool
         | Char Char
         | String String
         | List [AST]
         | AST :-: AST
--}

parseUntyped =
  try (fmap Symbol identifier)
    <|> try (fmap Int integer)
    <|> try (fmap Float float)
    <|> try (reserved "#t" >> return (Bool True))
    <|> try (reserved "#f" >> return (Bool False))
    <|> try (fmap Char charLiteral)
    <|> try (fmap String stringLiteral)
    <|> try (fmap List (parens $ many parseExpr))

parseTyped = do
  e <- parseUntyped
  reservedOp ":"
  t <- parseUntyped
  return (e :-: t)

parseExpr = try parseTyped <|> parseUntyped

parseToplevel = many parseExpr

parse = Text.Parsec.parse parseToplevel ""

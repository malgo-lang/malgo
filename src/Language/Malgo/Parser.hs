module Language.Malgo.Parser
  where

import           Language.Malgo.Syntax

import           Data.Functor.Identity (Identity)
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

parseUntyped = do
  pos <- getPosition
  try (fmap (Symbol pos) identifier)
    <|> try (fmap (Float pos) float)
    <|> try (fmap (Int pos) integer)
    <|> try (reserved "#t" >> return (Bool pos True))
    <|> try (reserved "#f" >> return (Bool pos False))
    <|> try (fmap (Char pos) charLiteral)
    <|> try (fmap (String pos) stringLiteral)
    <|> try (char '\'' >> identifier >>= \s -> return (Tree [Symbol pos "quote", Symbol (incSourceColumn pos 1) s]))
    <|> fmap (List pos) (brackets (many parseExpr))
    <|> fmap Tree (parens (many parseExpr))

parseTyped = parseUntyped >>=  \e -> reservedOp ":" >> getPosition >>= \pos -> parseUntyped >>= \t -> return (Typed pos e t)

parseExpr = try parseTyped <|> parseUntyped

parse = Text.Parsec.parse parseExpr "test"

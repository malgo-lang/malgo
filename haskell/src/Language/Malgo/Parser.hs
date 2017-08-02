module Language.Malgo.Parser
  where

import Language.Malgo.Syntax

import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language

lexer = Tok.makeTokenParser $ emptyDef {
    Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.identStart = letter <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.identLetter = alphaNum <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.reservedOpNames = ["->", ":"]
  , Tok.reservedNames = ["#t", "#f", "Unit", "Int", "Float", "Symbol", "Bool", "List"]
  }

integer = Tok.integer lexer
float = Tok.float lexer
parens = Tok.parens lexer
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
brackets = Tok.brackets lexer
lexeme = Tok.lexeme lexer

parseType = chainl1 parseAtomType parseFunT

parseAtomType =
  (reserved "Unit" >> return UnitT)
  <|> (reserved "Int" >> return IntT)
  <|> (reserved "Float" >> return FloatT)
  <|> (reserved "Symbol" >> return SymbolT)
  <|> (reserved "List" >> parseAtomType >>= return . ListT)
  <|> parens parseType

parseFunT = reservedOp "->" >> return FunT

parseExpr' = try (identifier >>= return . Symbol)
  <|> try (float >>= return . Float)
  <|> try (integer >>= return . Int)
  <|> try (reserved "#t" >> return (Bool True))
  <|> try (reserved "#f" >> return (Bool False))
  <|> (brackets (many parseExpr) >>= return . List)
  <|> (parens (many parseExpr) >>= return . Tree)

parseTyped = do
  e <- parseExpr'
  reservedOp ":"
  t <- parseType
  return (Typed e t)

parseExpr = try parseTyped <|> parseExpr'

parse = Text.Parsec.parse parseExpr ""

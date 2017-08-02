module Language.Malgo.Parser
  where

import Language.Malgo.Syntax

import Control.Applicative ((<*))

import Text.Parsec
import Text.Parsec.String
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
  <|> parseListT
  <|> parseParens
  where
    parseListT = do
      reserved "List"
      t <- parseAtomType
      return (ListT t)
    parseParens = do
      t <- parens (lexeme parseType)
      return t

parseFunT = reservedOp "->" >> return FunT

parseExpr' = try (identifier >>= \s -> return (Symbol s))
  <|> try (float >>= \f -> return (Float f))
  <|> try (integer >>= \i -> return (Int i))
  <|> try (reserved "#t" >> return (Bool True))
  <|> try (reserved "#f" >> return (Bool False))
  <|> parseList
  <|> parseTree
  where
    parseList = do
      xs <- brackets (many parseExpr)
      return (List xs)
    parseTree = do
      xs <- parens (many parseExpr)
      return (Tree xs)

parseTyped = do
  e <- parseExpr'
  reservedOp ":"
  t <- parseType
  return (Typed e t)

parseExpr = try parseTyped <|> parseExpr'

parse = Text.Parsec.parse parseExpr ""

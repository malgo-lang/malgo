module Language.Malgo.Parser
  where

import Language.Malgo.Syntax

import Control.Applicative ((<*))

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language

lexer = Tok.makeTokenParser $ emptyDef {
    Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.identStart = letter <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.identLetter = alphaNum <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.reservedOpNames = ["->", ":"]
  , Tok.reservedNames = ["Unit", "Int", "Float", "Symbol", "Bool"]
  }

integer = Tok.integer lexer
float = Tok.float lexer
parens = Tok.parens lexer
indentifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

parseType = chainl1 parseAtomType parseFunT

parseAtomType =
  (reserved "Unit" >> return UnitT)
  <|> (reserved "Int" >> return IntT)
  <|> (reserved "Float" >> return FloatT)
  <|> (reserved "Symbol" >> return SymbolT)
  <|> parseListT
  <|> parseVectorT
  where
    parseListT = do
      t <- parens parseType
      return (ListT t)
    parseVectorT = do
      t <- Tok.brackets lexer parseType
      return (VectorT t)

parseFunT = reservedOp "->" >> return FunT

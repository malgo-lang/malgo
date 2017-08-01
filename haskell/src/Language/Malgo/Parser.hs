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
  , Tok.reservedNames = ["Unit", "Int", "Float", "Symbol", "Bool", "List"]
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
  -- <|> parseVectorT
  <|> parseParens
  where
    parseListT = do
      reserved "List"
      t <- parseAtomType
      return (ListT t)
    -- parseVectorT = do
    --   reserved "Vector"
    --   t <- parseAtomType
    --   return (VectorT t)
    parseParens = do
      t <- parens parseType
      return t

parseFunT = reservedOp "->" >> return FunT

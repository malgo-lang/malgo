module Language.Malgo.Parser where

import           Language.Malgo.Syntax
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token     as Tok

lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = "--"
  , Tok.identStart = letter <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.identLetter = alphaNum <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.reservedOpNames = [":", "="]
  , Tok.reservedNames = ["def", "if", "else", "#t", "#f"]
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
commaSep = Tok.commaSep lexer
braces = Tok.braces lexer
semiSep = Tok.semiSep lexer
semi = Tok.semi lexer

parseDecl = try parseDef <|> parseDefun

parseDef = do
  reserved "def"
  name <- identifier
  reservedOp ":"
  ty <- parseType
  reservedOp "="
  val <- parseExpr
  return $ Def name ty val

parseDefun = do
  reserved "def"
  name <- identifier
  params <- parens (commaSep param)
  reservedOp ":"
  ty <- parseType
  reservedOp "="
  body <- parseExpr
  return $ Defun name ty params body
  where param = do
          name <- identifier
          reservedOp ":"
          ty <- parseType
          return (name, ty)

parseType = try (symbol "Int" >> return IntTy)
  <|> try (symbol "Float" >> return FloatTy)
  <|> try (symbol "Bool" >> return BoolTy)
  <|> try (symbol "Char" >> return CharTy)
  <|> try (symbol "String" >> return StringTy)

parseExpr = try parseCall
  <|> try parseVar
  <|> try parseLit
  <|> try parseIf
  <|> parens parseExpr
  <|> fmap Block (braces (semiSep parseExpr))

parseIf = do
  reserved "if"
  cond <- parseExpr
  then' <- parseExpr
  reserved "else"
  else' <- parseExpr
  return $ If cond then' else'

parseCall = do
  fun <- identifier
  args <- parens (commaSep parseExpr)
  return $ Call fun args

parseVar = identifier >>= return . Var

parseLit = try (fmap Int integer)
  <|> try (fmap Float float)
  <|> try (reserved "#t" >> return (Bool True))
  <|> try (reserved "#f" >> return (Bool False))
  <|> try (fmap Char charLiteral)
  <|> try (fmap String stringLiteral)

parseToplevel = semiSep parseDecl >>= \ast -> eof >> return ast

parse = Text.Parsec.parse parseToplevel ""

{-# LANGUAGE RankNTypes #-}
module Language.Malgo.Parser where

import           Data.Functor.Identity (Identity)
import           Language.Malgo.Syntax
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import qualified Text.Parsec.Token     as Tok

type Parser a = forall u. ParsecT String u Identity a

lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = "--"
  , Tok.identStart = letter <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.identLetter = alphaNum <|> oneOf "!$%&*+-./<=>?@^_~"
  , Tok.reservedOpNames = [":", "=", "+", "-", "*", "/"]
  , Tok.reservedNames = ["def", "if", "else", "#t", "#f"]
  }

table = [ [prefix "-" (\x -> Call "negate" [x]), prefix "+" id]
        , [binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
        , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
        ]

prefix name fun = Prefix (reservedOp name >> return fun)
postfix name fun = Postfix (reservedOp name >> return fun)
binary name fun assoc = Infix (reservedOp name >> return fun) assoc

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

parseDecl :: Parser Decl
parseDecl = try parseDefun <|> parseDef

parseDef :: Parser Decl
parseDef = do
  reserved "def"
  name <- identifier
  reservedOp ":"
  ty <- parseType
  reservedOp "="
  val <- parseExpr
  return $ Def name ty val

parseDefun :: Parser Decl
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

parseType :: Parser Type
parseType = try (symbol "Int" >> return IntTy)
  <|> try (symbol "Float" >> return FloatTy)
  <|> try (symbol "Bool" >> return BoolTy)
  <|> try (symbol "Char" >> return CharTy)
  <|> try (symbol "String" >> return StringTy)

parseTerm :: Parser Expr
parseTerm = try parseCall
  <|> try parseVar
  <|> try parseLit
  <|> try parseIf
  <|> parens parseExpr
  <|> fmap Block (braces (semiSep parseExpr))

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseTerm

parseIf :: Parser Expr
parseIf = do
  reserved "if"
  cond <- parseExpr
  then' <- parseExpr
  reserved "else"
  else' <- parseExpr
  return $ If cond then' else'

parseCall :: Parser Expr
parseCall = do
  fun <- identifier
  args <- parens (commaSep parseExpr)
  return $ Call fun args

parseVar :: Parser Expr
parseVar = identifier >>= return . Var

parseLit :: Parser Expr
parseLit = try (fmap Int integer)
  <|> try (fmap Float float)
  <|> try (reserved "#t" >> return (Bool True))
  <|> try (reserved "#f" >> return (Bool False))
  <|> try (fmap Char charLiteral)
  <|> try (fmap String stringLiteral)

parseToplevel :: Parser [Decl]
parseToplevel = semiSep parseDecl >>= \ast -> eof >> return ast

parse = Text.Parsec.parse parseToplevel ""

parseTest :: Show a => Parser a -> String -> IO ()
parseTest = Text.Parsec.parseTest

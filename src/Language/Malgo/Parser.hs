{-# LANGUAGE RankNTypes #-}
module Language.Malgo.Parser where

import           Data.Functor.Identity (Identity)
import           Language.Malgo.Syntax
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import qualified Text.Parsec.Token     as Tok

type Parser a = forall u. ParsecT String u Identity a

lexer :: forall u. Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = "--"
  , Tok.identStart = letter <|> char '_' -- <|> oneOf "!$&?@^_~"
  , Tok.identLetter = alphaNum <|> char '_' -- <|> oneOf "!$&?@^_~"
  , Tok.reservedOpNames = [":", "=", "+", "-", "*", "/", ";"]
  , Tok.reservedNames = ["unit", "def", "if", "else", "#t", "#f"]
  }

table = [ [prefix "-" (\x -> Call "negate" [x]), prefix "+" id]
        , [binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
        , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
        , [binary ";" Seq AssocRight]
        ]

prefix name fun = Prefix (reservedOp name >> return fun)
postfix name fun = Postfix (reservedOp name >> return fun)
binary name fun = Infix (reservedOp name >> return fun)

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
  (name, ty) <- parseVarWithAnn
  reservedOp "="
  val <- parseExpr
  return $ Def name ty val

parseDefun :: Parser Decl
parseDefun = do
  reserved "def"
  name <- identifier
  params <- parens (commaSep parseVarWithAnn)
  reservedOp ":"
  ty <- parseType
  reservedOp "="
  body <- parseExpr
  return $ Defun name ty params body

parseVar :: Parser Expr
parseVar = fmap Var identifier

parseVarWithAnn = do
  (Var name) <- parseVar
  reservedOp ":"
  ty <- parseType
  return (name, ty)

parseType :: Parser Type
parseType = (symbol "Int" >> return IntTy)
  <|> (symbol "Float" >> return FloatTy)
  <|> (symbol "Bool" >> return BoolTy)
  <|> (symbol "Char" >> return CharTy)
  <|> (symbol "String" >> return StringTy)
  <|> (symbol "Unit" >> return UnitTy)

parseTerm :: Parser Expr
parseTerm = try parseCall
  <|> try parseVar
  <|> try parseLit
  <|> try parseIf
  <|> parens parseExpr
  <|> braces parseExpr
  <?> "term"

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


parseLit :: Parser Expr
parseLit = try (fmap Int integer)
  <|> try (fmap Float float)
  <|> try (reserved "#t" >> return (Bool True))
  <|> try (reserved "#f" >> return (Bool False))
  <|> fmap Char charLiteral
  <|> fmap String stringLiteral
  <|> (reserved "unit" >> return Unit)

parseToplevel :: Parser [Decl]
parseToplevel = many parseDecl >>= \ast -> eof >> return ast

parse = Text.Parsec.parse parseToplevel ""

parseTest :: Show a => Parser a -> String -> IO ()
parseTest = Text.Parsec.parseTest

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Language.Malgo.Parser where

import           Data.Functor.Identity (Identity)
import qualified Data.Text             as T
import           Language.Malgo.Syntax
import           Text.Parsec           hiding (parse)
import qualified Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tok

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = "--"
  , Tok.identStart = letter <|> char '_' -- <|> oneOf "!$&?@^_~"
  , Tok.identLetter = alphaNum <|> char '_' -- <|> oneOf "!$&?@^_~"
  , Tok.reservedOpNames = [":", "=", "+", "-", "*", "/", ";", "==", "/=", "&&", "||", "<", "<=", ">", ">="]
  , Tok.reservedNames = ["let", "unit", "def", "if", "else", "#t", "#f"]
  }

table :: [[Operator String u Identity Expr]]
table = [ [prefix "-" (\x -> Call (mkName "negate") [x]), prefix "+" id]
        , [ binary "*" (BinOp Mul) AssocLeft
          , binary "/" (BinOp Div) AssocLeft
          , binary "==" (BinOp Eq) AssocNone
          , binary "/=" (\x y -> Call (mkName "not") [BinOp Eq x y]) AssocNone
          , binary "<=" (BinOp Le) AssocNone
          , binary "<" (BinOp Lt) AssocNone
          , binary ">=" (BinOp Ge) AssocNone
          , binary ">" (BinOp Gt) AssocNone
          ]
        , [ binary "+" (BinOp Add) AssocLeft
          , binary "-" (BinOp Sub) AssocLeft
          , binary "&&" (BinOp And) AssocLeft
          , binary "||" (BinOp Or) AssocLeft
          ]
        ]

prefix :: String -> (a -> a) -> Operator String u Identity a
prefix name fun = Prefix (reservedOp name >> return fun)

postfix :: String -> (a -> a) -> Operator String u Identity a
postfix name fun = Postfix (reservedOp name >> return fun)

binary
  :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
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

parseDecl :: ParsecT String u Identity Decl
parseDecl = try parseDefun <|> parseDef

parseDef :: ParsecT String u Identity Decl
parseDef = do
  reserved "def"
  (name, ty) <- parseVarWithAnn
  reservedOp "="
  val <- parseExpr
  return $ Def name ty val

parseDefun :: ParsecT String u Identity Decl
parseDefun = do
  reserved "def"
  name <- identifier
  params <- parens (commaSep parseVarWithAnn)
  reservedOp ":"
  ty <- parseType
  reservedOp "="
  body <- parseExpr
  return $ Defun (mkName name) ty params body

parseVar :: ParsecT String u Identity Expr
parseVar = fmap (Var . mkName) identifier

parseVarWithAnn :: ParsecT String u Identity (Name, Type)
parseVarWithAnn = do
  (Var name) <- parseVar
  reservedOp ":"
  ty <- parseType
  return (name, ty)

parseType :: ParsecT String u Identity Type
parseType = (symbol "Int" >> return IntTy)
  <|> (symbol "Float" >> return FloatTy)
  <|> (symbol "Bool" >> return BoolTy)
  <|> (symbol "Char" >> return CharTy)
  <|> (symbol "String" >> return StringTy)
  <|> (symbol "Unit" >> return UnitTy)

parseTerm :: ParsecT String u Identity Expr
parseTerm = try parseCall
  <|> try parseVar
  <|> try parseLit
  <|> try parseIf
  <|> parseLet
  <|> parens parseExpr
  <|> braces parseExpr

parseLet :: ParsecT String u Identity Expr
parseLet = do
  reserved "let"
  (name, ty) <- parseVarWithAnn
  reservedOp "="
  val <- parseExpr'
  return $ Let name ty val

parseExpr' :: ParsecT String u Identity Expr
parseExpr' = buildExpressionParser table parseTerm

parseExpr :: ParsecT String u Identity Expr
parseExpr = try (do
                    e1 <- parseExpr'
                    reservedOp ";"
                    e2 <- parseExpr
                    return (Seq e1 e2))
            <|> try (do
                        e <- parseExpr'
                        reservedOp ";"
                        return (Seq e Unit))
            <|> parseExpr'

parseIf :: ParsecT String u Identity Expr
parseIf = do
  reserved "if"
  cond <- parseExpr
  then' <- parseExpr
  reserved "else"
  else' <- parseExpr
  return $ If cond then' else'

parseCall :: ParsecT String u Identity Expr
parseCall = do
  fun <- identifier
  args <- parens (commaSep parseExpr)
  return $ Call (mkName fun) args


parseLit :: ParsecT String u Identity Expr
parseLit = try (fmap (Int . fromInteger) integer)
  <|> try (fmap Float float)
  <|> try (reserved "#t" >> return (Bool True))
  <|> try (reserved "#f" >> return (Bool False))
  <|> fmap Char charLiteral
  <|> fmap String stringLiteral
  <|> (reserved "unit" >> return Unit)

parseToplevel :: ParsecT String u Identity [Decl]
parseToplevel = many parseDecl >>= \ast -> eof >> return ast

parse :: String -> Either ParseError [Decl]
parse = Text.Parsec.parse parseToplevel ""

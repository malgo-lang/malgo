module Language.Malgo.Parser where

import           Control.Applicative   hiding (Const)
import           Data.Functor.Identity (Identity)
import           Debug.Trace
import           Language.Malgo.Syntax
import           Language.Malgo.Utils
import           Text.Parsec           hiding (many, parse, (<|>))
import qualified Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Pos
import qualified Text.Parsec.Token     as Tok

parseToplevel :: ParsecT String u Identity Expr
parseToplevel = whiteSpace >> parseExpr >>= \ast -> eof >> return ast

-- parse :: SourceName -> String -> Either ParseError Expr
-- parse = Text.Parsec.parse parseToplevel

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = "--"
  , Tok.identStart = letter <|> oneOf "!?@_"
  , Tok.identLetter = alphaNum <|> oneOf "!?@_"
  , Tok.reservedOpNames = [ ":", "=", "+", "-", "*"
                          , "/", "%", ";", "==", "<>"
                          , "&&", "||", "<", ">", "<=", ">="]
  , Tok.reservedNames = ["let", "val", "fun", "if", "true", "false", "seq", "unit"]
  }

getInfo :: ParsecT String u Identity Info
getInfo = do
  pos <- getPosition
  return (Info (sourceName pos, sourceLine pos, sourceColumn pos))

parseExpr =
  parseVar
  <|> parens (parseBinOp
              <|> parseCall
              <|> parseIf
              <|> parseLet
              <|> parseSeq)
  <|> parseConst

parseBinOp = do
  info <- getInfo
  op <- parseOp
  e1 <- parseExpr
  es <- many1 parseExpr <?> "one or more arguments for (" ++ show (pretty op) ++ ")"
  return $ foldl (BinOp info op) e1 es

parseOp = (reservedOp "+" >> return Add)
  <|> (reservedOp "-" >> return Sub)
  <|> (reservedOp "*" >> return Mul)
  <|> (reservedOp "/" >> return Div)
  <|> (reservedOp "%" >> return Mod)
  <|> (reservedOp "==" >> return Eq)
  <|> (reservedOp "<>" >> return Neq)
  <|> (reservedOp "<" >> return Lt)
  <|> (reservedOp ">" >> return Gt)
  <|> (reservedOp "<=" >> return Le)
  <|> (reservedOp ">=" >> return Ge)
  <|> (reservedOp "&&" >> return And)
  <|> (reservedOp "||" >> return Or)

parseCall = do
  info <- getInfo
  Call info <$> parseName <*> many parseExpr

parseIf = do
  info <- getInfo
  reserved "if"
  c <- parseExpr
  t <- parseExpr
  f <- parseExpr
  return (If info c t f)

parseSeq = do
  info <- getInfo
  reserved "seq"
  parseSeq' info

parseSeq' info = do
  e1 <- parseExpr
  es <- many1 parseExpr
  return $ foldr (Seq info) e1 es

parseLet = do
  info <- getInfo
  reserved "let"
  decls <- parens (many1 (parens parseDecl))
  info' <- getInfo
  body <- try (parseSeq' info') <|> parseExpr
  return (Let info decls body)

parseDecl = parseValDec <|> parseFunDec

parseValDec = do
  info <- getInfo
  reserved "val"
  (name, typ) <- parseField
  val <- parseExpr
  return (ValDec info name typ val)

parseFunDec = do
  info <- getInfo
  reserved "fun"
  (fnName, retTy):params <- parens (many1 parseField)
  info' <- getInfo
  body <- try (parseSeq' info') <|> parseExpr
  return (FunDec info fnName params retTy body)

parseName :: ParsecT String u Identity Name
parseName = Name <$> identifier

parseType = NameTy <$> (parseName <|> (reserved "unit" >> return (Name "unit")))

parseField = (,) <$> parseName <*> (reservedOp ":" *> parseType)

parseConst = try (Float <$> getInfo <*> float)
  <|> (Int <$> getInfo <*> integer)
  <|> (Char <$> getInfo <*> charLiteral)
  <|> (String <$> getInfo <*> stringLiteral)
  <|> (Bool <$> getInfo <*> (reserved "true" >> return True))
  <|> (Bool <$> getInfo <*> (reserved "false" >> return False))
  <|> fmap Unit (reserved "unit" >> getInfo)

parseVar = Var <$> getInfo <*> parseName

integer :: ParsecT String u Identity Integer
integer = Tok.integer lexer
float :: ParsecT String u Identity Double
float = Tok.float lexer
parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = Tok.parens lexer
identifier :: ParsecT String u Identity String
identifier = Tok.identifier lexer
reserved :: String -> ParsecT String u Identity ()
reserved = Tok.reserved lexer
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = Tok.reservedOp lexer
brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = Tok.brackets lexer
lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = Tok.lexeme lexer
stringLiteral :: ParsecT String u Identity String
stringLiteral = Tok.stringLiteral lexer
charLiteral :: ParsecT String u Identity Char
charLiteral = Tok.charLiteral lexer
symbol :: String -> ParsecT String u Identity String
symbol = Tok.symbol lexer
commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = Tok.commaSep lexer
braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = Tok.braces lexer
semiSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep = Tok.semiSep lexer
semi :: ParsecT String u Identity String
semi = Tok.semi lexer
whiteSpace :: ParsecT String u Identity ()
whiteSpace = Tok.whiteSpace lexer

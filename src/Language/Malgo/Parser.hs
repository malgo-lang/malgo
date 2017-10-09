module Language.Malgo.Parser where

import           Control.Applicative
import           Data.Functor.Identity (Identity)
import           Language.Malgo.Syntax
import           Text.Parsec           hiding (many, parse, (<|>))
import qualified Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Pos       ()
import qualified Text.Parsec.Token     as Tok

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = "--"
  , Tok.identStart = letter <|> char '_' -- <|> oneOf "!$&?@^_~"
  , Tok.identLetter = alphaNum <|> char '_' -- <|> oneOf "!$&?@^_~"
  , Tok.reservedOpNames = [":", "=", "+", "-", "*", "/", ";", "==", "/=", "&&", "||", "<", "<=", ">", ">="]
  , Tok.reservedNames = ["extern", "let", "unit", "def", "if", "else", "#t", "#f"]
  }

table :: [[Operator String u Identity Expr]]
table = [ [ prefix "-" (\pos x -> Call pos (mkName "negate") [x])
          , prefix "+" (flip const)]
        , [ binary "*" (`BinOp` Mul) AssocLeft
          , binary "/" (`BinOp` Div) AssocLeft
          , binary "==" (`BinOp` Eq) AssocNone
          , binary "/=" (`BinOp` Neq) AssocNone
          , binary "<=" (`BinOp` Le) AssocNone
          , binary "<" (`BinOp` Lt) AssocNone
          , binary ">=" (`BinOp` Ge) AssocNone
          , binary ">" (`BinOp` Gt) AssocNone
          ]
        , [ binary "+" (`BinOp` Add) AssocLeft
          , binary "-" (`BinOp` Sub) AssocLeft
          , binary "&&" (`BinOp` And) AssocLeft
          , binary "||" (`BinOp` Or) AssocLeft
          ]
        ]

prefix :: String -> (SourcePos -> a -> a) -> Operator String u Identity a
prefix name fun = Prefix (getPosition >>= \pos -> reservedOp name >> return (fun pos))

postfix :: String -> (SourcePos -> a -> a) -> Operator String u Identity a
postfix name fun = Postfix (getPosition >>= \pos -> reservedOp name >> return (fun pos))

binary :: String -> (SourcePos -> a -> a -> a) -> Assoc -> Operator String u Identity a
binary name fun = Infix (getPosition >>= \pos -> reservedOp name >> return (fun pos))

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

parseDecl :: ParsecT String u Identity Decl
parseDecl = try parseDefun <|> parseDef <|> try parseExDefun <|> parseExDef

parseDef :: ParsecT String u Identity Decl
parseDef = do
  pos <- getPosition
  reserved "def"
  (name, ty) <- parseVarWithAnn
  reservedOp "="
  val <- parseExpr
  return $ Def pos name ty val

parseDefun :: ParsecT String u Identity Decl
parseDefun = do
  pos <- getPosition
  reserved "def"
  name <- identifier
  params <- parens (commaSep parseVarWithAnn)
  reservedOp ":"
  ty <- parseType
  reservedOp "="
  body <- parseExpr
  return $ Defun pos (mkName name) ty params body

parseExDefun :: ParsecT String u Identity Decl
parseExDefun = do
  pos <- getPosition
  reserved "extern"
  name <- identifier
  params <- parens (commaSep parseVarWithAnn)
  reservedOp ":"
  ty <- parseType
  return $ ExDefun pos (mkName name) ty params

parseExDef :: ParsecT String u Identity Decl
parseExDef = do
  pos <- getPosition
  reserved "extern"
  (name, ty) <- parseVarWithAnn
  return $ ExDef pos name ty

parseVar :: ParsecT String u Identity Expr
parseVar = do
  pos <- getPosition
  name <- identifier
  return (Var pos (mkName name))

parseVarWithAnn :: ParsecT String u Identity (Name, Type)
parseVarWithAnn = do
  (Var _ name) <- parseVar
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
  <|> parseVar
  <|> parseLit
  <|> parseIf
  <|> parseLet
  <|> parens parseExpr
  <|> braces parseExpr

parseLet :: ParsecT String u Identity Expr
parseLet = do
  pos <- getPosition
  reserved "let" >> Let pos <$> fmap mkName identifier <*> (reservedOp ":" >> parseType) <*> (reservedOp "=" >> parseExpr')

parseExpr' :: ParsecT String u Identity Expr
parseExpr' = buildExpressionParser table parseTerm

parseExpr :: ParsecT String u Identity Expr
parseExpr =
  try (do e1 <- parseExpr'
          pos <- getPosition
          reservedOp ";"
          e2 <- try parseExpr <|> pure (Unit pos)
          return $ Seq pos e1 e2)
  <|> parseExpr'

parseIf :: ParsecT String u Identity Expr
parseIf = getPosition >>= \pos -> reserved "if" >> If pos <$> parseExpr <*> parseExpr <*> (reserved "else" >> parseExpr)

parseCall :: ParsecT String u Identity Expr
parseCall = Call <$> getPosition <*> fmap mkName identifier <*> parens (commaSep parseExpr)

parseLit :: ParsecT String u Identity Expr
parseLit = try (Float <$> getPosition <*> float)
  <|> (Int <$> getPosition <*> fmap fromInteger integer)
  <|> (getPosition >>= \pos -> reserved "#t" >> return (Bool pos True))
  <|> (getPosition >>= \pos -> reserved "#f" >> return (Bool pos False))
  <|> (Char <$> getPosition <*> charLiteral)
  <|> (String <$> getPosition <*> stringLiteral)
  <|> (getPosition >>= \pos -> reserved "unit" >> return (Unit pos))

parseToplevel :: ParsecT String u Identity [Decl]
parseToplevel = many parseDecl >>= \ast -> eof >> return ast

parse :: String -> Either ParseError [Decl]
parse = Text.Parsec.parse parseToplevel ""

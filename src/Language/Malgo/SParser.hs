module Language.Malgo.SParser where

import           Control.Applicative   hiding (Const)
import           Data.Functor.Identity (Identity)
import           Debug.Trace
import           Language.Malgo.Syntax
import           Language.Malgo.Types
import           Text.Parsec           hiding (many, parse, (<|>))
import qualified Text.Parsec
-- import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Pos
import qualified Text.Parsec.Token     as Tok

parseToplevel = whiteSpace >> many parseDecl >>= \ast -> eof >> return ast

parse :: SourceName -> String -> Either ParseError [Decl]
parse = Text.Parsec.parse parseToplevel

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = ";"
  , Tok.identStart = letter <|> oneOf "!?@_"
  , Tok.identLetter = alphaNum <|> oneOf "!?@_"
  , Tok.reservedOpNames = [ ":", "=", "+", "-", "*"
                          , "/", "%", ";", "==", "!="
                          , "&&", "||", "<", ">", "<=", ">="]
  , Tok.reservedNames = ["extern", "def", "if", "else", "#t", "#f"]
  }

getInfo = Info <$> getPosition

parseDecl :: ParsecT String u Identity Decl
parseDecl = parens (try parseDefVar <|> parseDefFun <|> try parseExVar <|> parseExFun)

parseDefVar = do
  info <- getInfo
  reserved "def"
  (name, typ) <- parseTypedName
  val <- parseConstExpr
  return $ DefVar info name typ val

parseDefFun :: ParsecT String u Identity Decl
parseDefFun = do
  info <- getInfo
  reserved "def"
  (name, ty, params') <- parens $ do
        (name, ty) <- parseTypedName
        params' <- many parseTypedName
        return (name, ty, params')
  let params = case params' of
        [] -> [(Name "_", UnitTy)]
        _  -> params'

  body <- parseExpr
  return $ DefFun info name ty params body

parseExVar :: ParsecT String u Identity Decl
parseExVar = do
  info <- getInfo
  reserved "extern"
  (name, typ) <- parseTypedName
  return $ ExVar info name typ

parseExFun :: ParsecT String u Identity Decl
parseExFun = do
  info <- getInfo
  reserved "extern"
  (name, ty, params') <- parens $ do
        (name, ty) <- parseTypedName
        params' <- many parseTypedName
        return (name, ty, params')

  let params = case params' of
        [] -> [(Name "_", UnitTy)]
        _  -> params'

  return $ ExFun info name ty params

parseName :: ParsecT String u Identity Name
parseName = Name <$> identifier

parseType :: ParsecT String u Identity Type
parseType = (symbol "Int" >> return IntTy)
  <|> (symbol "Float" >> return FloatTy)
  <|> (symbol "Bool" >> return BoolTy)
  <|> (symbol "Char" >> return CharTy)
  <|> (symbol "String" >> return StringTy)
  <|> (symbol "Unit" >> return UnitTy)

parseTypedName :: ParsecT String u Identity (Name, Type)
parseTypedName = (,) <$> parseName <*> (reservedOp ":" >> parseType)

parseConst :: ParsecT String u Identity Const
parseConst = try (Float <$> getInfo <*> float)
  <|> (Int <$> getInfo <*> integer)
  <|> (Char <$> getInfo <*> charLiteral)
  <|> (String <$> getInfo <*> stringLiteral)
  <|> (Bool <$> getInfo <*> (reserved "#t" >> return True))
  <|> (Bool <$> getInfo <*> (reserved "#f" >> return False))
  <|> try (fmap Unit (parens whiteSpace >> getInfo))

parseConstExpr = try $ parseBinOp CBinOp parseConst
  <|> parseConst

parseBinOp fn factor = parens $ do
  info <- getInfo
  op <- parseOp
  x <- factor
  y <- factor
  return (fn info op x y)

parseOp = (reservedOp "+" >> return Add)
  <|> (reservedOp "-" >> return Sub)
  <|> (reservedOp "*" >> return Mul)
  <|> (reservedOp "/" >> return Div)
  <|> (reservedOp "%" >> return Mod)
  <|> (reservedOp "==" >> return Eq)
  <|> (reservedOp "/=" >> return Neq)
  <|> (reservedOp "<" >> return Lt)
  <|> (reservedOp ">" >> return Gt)
  <|> (reservedOp "<=" >> return Le)
  <|> (reservedOp ">=" >> return Ge)
  <|> (reservedOp "&&" >> return And)
  <|> (reservedOp "||" >> return Or)

parseExpr = fmap Const parseConst
  <|> (Var <$> getInfo <*> parseName)
  <|> try parseCall
  <|> try (parseBinOp BinOp parseExpr)
  <|> try (parens (do
                      info <- getInfo
                      reservedOp "-"
                      x <- parseExpr
                      return $ BinOp info Sub (Const (Int dummyInfo 0)) x))
  <|> try (parens (If <$> getInfo
                   <*> (reserved "if" >> parseExpr)
                   <*> parseExpr
                   <*> parseExpr))
  <|> try parseLet
  <|> parseSeq
  where
    parseCall = parens $ do
      i <- getInfo
      fn <- parseName
      args <- many parseExpr
      case args of
        [] -> return (Call i fn [Const $ Unit i])
        _  -> return (Call i fn args)

parseLet = parens $ do
  info <- getInfo
  reserved "let"
  defs <- parens (many1 parseDef)
  body <- parseExpr
  return $ makeLet info defs body
  where parseDef = (,,) <$> parseName
          <*> (reservedOp ":" >> parseType)
          <*> parseExpr
        makeLet info [(name, typ, val)] body =
          Let info name typ val body
        makeLet info ((name, typ, val):xs) body =
          Let info name typ val (makeLet info xs body)
        makeLet info _ _ =
          error $ show info ++ ": the number of let definitions must be one or more"

parseSeq = try (parens $ reserved "seq" >> parseSeq')
           <|> braces parseSeq'
  where parseSeq' =  do
          info <- getInfo
          e <- many1 parseExpr
          return $ foldr1 (Seq info) e

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

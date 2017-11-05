module Language.Malgo.Parser where

import           Control.Applicative   hiding (Const)
import           Data.Functor.Identity (Identity)
import           Language.Malgo.Syntax
import           Language.Malgo.Types
import           Text.Parsec           hiding (many, parse, (<|>))
import qualified Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Pos
import qualified Text.Parsec.Token     as Tok

parseToplevel :: ParsecT String u Identity [Decl]
parseToplevel = whiteSpace >> many parseDecl >>= \ast -> eof >> return ast

parse :: SourceName -> String -> Either ParseError [Decl]
parse = Text.Parsec.parse parseToplevel

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = "--"
  , Tok.identStart = letter <|> oneOf "!?@_"
  , Tok.identLetter = alphaNum <|> oneOf "!?@_"
  , Tok.reservedOpNames = [ ":", "=", "+", "-", "*"
                          , "/", "%", ";", "==", "!="
                          , "&&", "||", "<", ">", "<=", ">="]
  , Tok.reservedNames = ["extern", "var", "fun", "if", "else", "#t", "#f"]
  }

prefix
  :: String -> (Info -> a -> a) -> Operator String u Identity a
prefix name fun = Prefix $
  getPosition >>= \pos -> reservedOp name >> return (fun (Info pos))

postfix
  :: String -> (Info -> a -> a) -> Operator String u Identity a
postfix name fun = Postfix $
  getPosition >>= \pos -> reservedOp name >> return (fun (Info pos))

binary
  :: String
     -> (Info -> a -> a -> a)
     -> Assoc
     -> Operator String u Identity a
binary name fun = Infix $
  getPosition >>= \pos -> reservedOp name >> return (fun (Info pos))

opTable
  :: (Info -> Op -> a -> a -> a)
     -> (Const -> a) -> [[Operator String u Identity a]]
opTable binop cons =
  [ [ prefix "-" (\info x -> binop info Sub (cons (Int info 0)) x)
    -- , prefix "+" (flip const)
    ]
  , [ binary "*" (`binop` Mul) AssocLeft
    , binary "/" (`binop` Div) AssocLeft
    , binary "%" (`binop` Mod) AssocLeft
    ]
  , [ binary "+" (`binop` Add) AssocLeft
    , binary "-" (`binop` Sub) AssocLeft
    ]
  , [ binary "==" (`binop` Eq) AssocNone
    , binary "/=" (`binop` Neq) AssocNone
    , binary "<=" (`binop` Le) AssocNone
    , binary "<" (`binop` Lt) AssocNone
    , binary ">=" (`binop` Ge) AssocNone
    , binary ">" (`binop` Gt) AssocNone
    ]
  , [ binary "&&" (`binop` And) AssocLeft
    , binary "||" (`binop` Or) AssocLeft
    ]
  ]

getInfo :: ParsecT String u Identity Info
getInfo = Info <$> getPosition

parseDecl :: ParsecT String u Identity Decl
parseDecl = try parseDefVar <|> parseDefFun <|> try parseExFun <|> parseExVar

parseDefVar :: ParsecT String u Identity Decl
parseDefVar = do
  info <- getInfo
  reserved "var"
  (name, typ) <- parseTypedName
  reservedOp "="
  val <- parseConstExpr
  return $ DefVar info name typ val

parseDefFun :: ParsecT String u Identity Decl
parseDefFun = do
  info <- getInfo
  reserved "fun"
  name <- parseName
  params' <- parens (commaSep parseTypedName)
  let params = case params' of
        [] -> [(Name "_", UnitTy)]
        _  -> params'
  reservedOp ":"
  ty <- parseType
  reservedOp "="
  body <- parseExpr
  return $ DefFun info name ty params body

parseExVar :: ParsecT String u Identity Decl
parseExVar = do
  info <- getInfo
  reserved "extern"
  reserved "var"
  (name, typ) <- parseTypedName
  return $ ExVar info name typ

parseExFun :: ParsecT String u Identity Decl
parseExFun = do
  info <- getInfo
  reserved "extern"
  reserved "fun"
  name <- parseName
  params' <- parens (commaSep parseTypedName)
  let params = case params' of
        [] -> [(Name "_", UnitTy)]
        _  -> params'
  reservedOp ":"
  typ <- parseType
  return $ ExFun info name typ params

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

parseConstExpr :: ParsecT String u Identity Const
parseConstExpr = buildExpressionParser (opTable CBinOp id) parseConst

parseExpr :: ParsecT String u Identity Expr
parseExpr = buildExpressionParser (opTable BinOp Const) $
  fmap Const parseConst
  <|> try parseCall
  <|> (Var <$> getInfo <*> parseName)
  <|> (If <$> getInfo
        <*> (reserved "if" >> parseExpr)
        <*> parseBlock
        <*> (reserved "else" >> parseBlock))
  <|> parseLet
  <|> try (fmap (Const . Unit) (braces whiteSpace >> getInfo))
  <|> parseBlock
  <|> parens parseExpr
  where
    parseCall = do
      i <- getInfo
      fn <- parseName
      args <- parens (commaSep parseExpr)
      case args of
        [] -> return (Call i fn [Const $ Unit i])
        _  -> return (Call i fn args)

parseLet :: ParsecT String u Identity Expr
parseLet = do
  info <- getInfo
  reserved "let"
  defs <- many1 parseDef
  blk <- parseBlock
  return $ makeLet info defs blk
  where makeLet info [(name, typ, val)] body =
          Let info name typ val body
        makeLet info ((name, typ, val):xs) body =
          Let info name typ val (makeLet info xs body)
        makeLet info _ _ =
          error $ show info ++ ": the number of let definitions must be one or more"
        parseDef = (,,) <$> parseName <*> (reservedOp ":" >> parseType) <*> (reservedOp "=" >> parseExpr)

parseBlock :: ParsecT String u Identity Expr
parseBlock = braces parseSeq

parseSeq :: ParsecT String u Identity Expr
parseSeq = try (do e1 <- parseExpr
                   info <- getInfo
                   reservedOp ";"
                   e2 <- try parseSeq <|> return (Const $ Unit info)
                   return $ Seq info e1 e2)
  <|> parseExpr

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

{-# LANGUAGE RankNTypes       #-}
module Language.Malgo.Lexer where

import           Data.Functor.Identity (Identity)
import           Data.String
import           Prelude               hiding (EQ, GT, LT)
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Pos       ()
import qualified Text.Parsec.Token     as Tok

import           Language.Malgo.Utils

data Tag = LET
         | IN
         | END
         | VAL
         | FUN
         | EXTERN
         | LPAREN
         | RPAREN
         | COMMA
         | COLON
         | SEMICOLON
         | EQUAL
         | IF
         | THEN
         | ELSE
         | PLUS
         | PLUS_DOT
         | MINUS
         | MINUS_DOT
         | ASTERISK
         | ASTERISK_DOT
         | SLASH
         | SLASH_DOT
         | PERCENT
         | ARROW
         | EQ
         | NEQ
         | LT
         | GT
         | LE
         | GE
         | AND
         | OR
         | ID { _id :: Name}
         | INT { _int :: Integer }
         | FLOAT { _float :: Double}
         | BOOL { _bool :: Bool}
         | CHAR { _char :: Char}
         | STRING { _str :: String}
  deriving (Eq, Show)

newtype Token = Token (Info, Tag)
  deriving (Eq, Show)

_info :: Token -> Info
_info (Token a) = fst a

_tag :: Token -> Tag
_tag (Token a) = snd a

type Lexer a = forall u. ParsecT String u Identity a

getInfo :: Lexer Info
getInfo = do
  pos <- getPosition
  return (Info (sourceName pos, sourceLine pos, sourceColumn pos))

lexer' :: Tok.GenTokenParser String u Identity
lexer' = Tok.makeTokenParser $ emptyDef {
  Tok.commentLine = "--"
  , Tok.identStart = letter <|> oneOf "!?@_"
  , Tok.identLetter = alphaNum <|> oneOf "!?@_"
  , Tok.reservedOpNames = [ "+.", "-.", "*.", "/."
                          , ":", "=", "+", "-", "*"
                          , "->"
                          , "/", "%", ";", "==", "<>"
                          , "&&", "||", "<", ">", "<=", ">="]
  , Tok.reservedNames = [ "let", "in", "end"
                        , "val", "fun", "extern"
                        , "if", "then", "else"
                        , "true", "false"]
  }

integer :: ParsecT String u Identity Integer
integer = Tok.integer lexer'
natural :: ParsecT String u Identity Integer
natural = Tok.natural lexer'
float :: ParsecT String u Identity Double
float = Tok.float lexer'
parens ::
  ParsecT String u Identity a -> ParsecT String u Identity a
parens = Tok.parens lexer'
identifier :: ParsecT String u Identity String
identifier = Tok.identifier lexer'
reserved :: String -> ParsecT String u Identity ()
reserved = Tok.reserved lexer'
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = Tok.reservedOp lexer'
brackets ::
  ParsecT String u Identity a -> ParsecT String u Identity a
brackets = Tok.brackets lexer'
lexeme ::
  ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = Tok.lexeme lexer'
stringLiteral :: ParsecT String u Identity String
stringLiteral = Tok.stringLiteral lexer'
charLiteral :: ParsecT String u Identity Char
charLiteral = Tok.charLiteral lexer'
symbol :: String -> ParsecT String u Identity String
symbol = Tok.symbol lexer'
commaSep ::
  ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = Tok.commaSep lexer'
braces ::
  ParsecT String u Identity a -> ParsecT String u Identity a
braces = Tok.braces lexer'
semiSep ::
  ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep = Tok.semiSep lexer'
semi :: ParsecT String u Identity String
semi = Tok.semi lexer'
whiteSpace :: ParsecT String u Identity ()
whiteSpace = Tok.whiteSpace lexer'
lparen :: ParsecT String u Identity String
lparen = symbol "("
rparen :: ParsecT String u Identity String
rparen = symbol ")"

keyword :: Info -> String -> Tag -> Lexer Token
keyword info word tag = reserved word >> return (Token (info, tag))
op :: Info -> String -> Tag -> Lexer Token
op info sym tag = reservedOp sym >> return (Token (info, tag))

lexer :: Lexer Token
lexer = do
  info <- getInfo
  keyword info "let" LET
    <|> keyword info "in" IN
    <|> keyword info "end" END
    <|> keyword info "val" VAL
    <|> keyword info "fun" FUN
    <|> keyword info "extern" EXTERN
    <|> keyword info "if" IF
    <|> keyword info "then" THEN
    <|> keyword info "else" ELSE
    <|> keyword info "true" (BOOL True)
    <|> keyword info "false" (BOOL False)
    <|> (lparen >> return (Token (info, LPAREN)))
    <|> (rparen >> return (Token (info, RPAREN)))
    <|> op info ":" COLON
    <|> op info ";" SEMICOLON
    <|> op info "," COMMA
    <|> op info "==" EQ
    <|> op info "=" EQUAL
    <|> op info "<>" NEQ
    <|> op info "<" LT
    <|> op info ">" GT
    <|> op info "<=" LE
    <|> op info ">=" GE
    <|> op info "+." PLUS_DOT
    <|> op info "-." MINUS_DOT
    <|> op info "*." ASTERISK_DOT
    <|> op info "/." SLASH_DOT
    <|> op info "%" PERCENT
    <|> op info "+" PLUS
    <|> op info "-" MINUS
    <|> op info "*" ASTERISK
    <|> op info "/" SLASH
    <|> op info "%" PERCENT
    <|> op info "&&" AND
    <|> op info "||" OR
    <|> op info "->" ARROW
    <|> fmap (\str -> Token (info, ID (fromString str))) identifier
    <|> try (fmap (\f -> Token (info, FLOAT f)) float)
    <|> fmap (\n -> Token (info, INT n)) natural
    <|> fmap (\c -> Token (info, CHAR c)) charLiteral
    <|> fmap (\s -> Token (info, STRING s)) stringLiteral

lexing :: SourceName -> String -> Either ParseError [Token]
lexing = parse (whiteSpace >> many lexer >>= \toks -> eof >> return toks)

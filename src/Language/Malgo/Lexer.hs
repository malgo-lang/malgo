{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE Strict                #-}
module Language.Malgo.Lexer ( Token(..), Tag(..), _info, _tag, lexing ) where

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.Prelude  hiding ( EQ
                                                , LT
                                                , GT
                                                )
import           Text.Parsec             hiding ( many
                                                , (<|>)
                                                )
import           Text.Parsec.Pos                ( )
import qualified Text.Parsec.Token             as Tok

data Tag
    = LET
    | IN
    | END
    | VAL
    | FUN
    | TYPE
    | EXTERN
    | LPAREN
    | RPAREN
    | LBRACK
    | RBRACK
    | LBRACE
    | RBRACE
    | COMMA
    | COLON
    | SEMICOLON
    | EQUAL
    | SEMICOLON_EQUAL
    | FN
    | IF
    | THEN
    | ELSE
    | DOT
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
    | ARRAY
    | LARROW
    | ID { _id :: String }
    | INT { _int :: Integer }
    | FLOAT { _float :: Double }
    | BOOL { _bool :: Bool }
    | CHAR { _char :: Char }
    | STRING { _str :: String }
    | TY_INT
    | TY_FLOAT
    | TY_BOOL
    | TY_CHAR
    | TY_STRING
    deriving (Eq, Show)

newtype Token =
    Token (Info, Tag)
    deriving (Eq, Show)

_info :: Token -> Info
_info (Token a) = fst a

_tag :: Token -> Tag
_tag (Token a) = snd a

-- type Lexer a = forall u. ParsecT String u Identity a

getInfo :: Monad m => ParsecT s u m Info
getInfo = do
  pos <- getPosition
  pure (Info (toText $ sourceName pos, sourceLine pos, sourceColumn pos))

lexer' :: Stream s m Char => Tok.GenTokenParser s u m
lexer' = Tok.makeTokenParser Tok.LanguageDef
  { Tok.nestedComments  = True
  , Tok.opStart         = oneOf ".+-*/:=%;<&|>"
  , Tok.opLetter        = oneOf ".+-*/:=%;<&|>"
  , Tok.caseSensitive   = True
  , Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.identStart      = letter <|> oneOf "!?@_#"
  , Tok.identLetter     = alphaNum <|> oneOf "!?@_"
  , Tok.reservedOpNames = [ "."
                          , "+."
                          , "-."
                          , "*."
                          , "/."
                          , ":"
                          , "="
                          , ":="
                          , "+"
                          , "-"
                          , "*"
                          , "->"
                          , "/"
                          , "%"
                          , ";"
                          , "=="
                          , "<>"
                          , "&&"
                          , "||"
                          , "<"
                          , ">"
                          , "<="
                          , ">="
                          , "<-"
                          ]
  , Tok.reservedNames   = [ "let"
                          , "in"
                          , "end"
                          , "val"
                          , "fun"
                          , "type"
                          , "extern"
                          , "fn"
                          , "if"
                          , "then"
                          , "else"
                          , "true"
                          , "false"
                          , "array"
                          , "Int"
                          , "Float"
                          , "Bool"
                          , "Char"
                          , "String"
                          ]
  }

keyword :: Stream s m Char => Info -> String -> Tag -> ParsecT s u m Token
keyword info word tag = reserved word >> pure (Token (info, tag))
  where reserved = Tok.reserved lexer'

op :: Stream s m Char => Info -> String -> Tag -> ParsecT s u m Token
op info sym tag = reservedOp sym >> pure (Token (info, tag))
  where reservedOp = Tok.reservedOp lexer'

lexer :: Stream s m Char => ParsecT s u m Token
lexer = do
  info <- getInfo
  keyword info "let" LET
    <|> keyword info "in"     IN
    <|> keyword info "end"    END
    <|> keyword info "val"    VAL
    <|> keyword info "fun"    FUN
    <|> keyword info "type"   TYPE
    <|> keyword info "extern" EXTERN
    <|> keyword info "fn"     FN
    <|> keyword info "if"     IF
    <|> keyword info "then"   THEN
    <|> keyword info "else"   ELSE
    <|> keyword info "true"   (BOOL True)
    <|> keyword info "false"  (BOOL False)
    <|> keyword info "array"  ARRAY
    <|> keyword info "Int"    TY_INT
    <|> keyword info "Float"  TY_FLOAT
    <|> keyword info "Bool"   TY_BOOL
    <|> keyword info "Char"   TY_CHAR
    <|> keyword info "String" TY_STRING
    <|> (lparen >> pure (Token (info, LPAREN)))
    <|> (rparen >> pure (Token (info, RPAREN)))
    <|> (lbrack >> pure (Token (info, LBRACK)))
    <|> (rbrack >> pure (Token (info, RBRACK)))
    <|> (lbrace >> pure (Token (info, LBRACE)))
    <|> (rbrace >> pure (Token (info, RBRACE)))
    <|> op info ":=" SEMICOLON_EQUAL
    <|> op info ":"  COLON
    <|> op info ";"  SEMICOLON
    <|> op info ","  COMMA
    <|> op info "==" EQ
    <|> op info "="  EQUAL
    <|> op info "<>" NEQ
    <|> op info "<"  LT
    <|> op info ">"  GT
    <|> op info "<=" LE
    <|> op info ">=" GE
    <|> op info "."  DOT
    <|> op info "+." PLUS_DOT
    <|> op info "-." MINUS_DOT
    <|> op info "*." ASTERISK_DOT
    <|> op info "/." SLASH_DOT
    <|> op info "%"  PERCENT
    <|> op info "+"  PLUS
    <|> op info "-"  MINUS
    <|> op info "*"  ASTERISK
    <|> op info "/"  SLASH
    <|> op info "%"  PERCENT
    <|> op info "&&" AND
    <|> op info "||" OR
    <|> op info "->" ARROW
    <|> op info "<-" LARROW
    <|> fmap (\str -> Token (info, ID str)) identifier
    <|> try (fmap (\f -> Token (info, FLOAT f)) float)
    <|> fmap (\n -> Token (info, INT n))    natural
    <|> fmap (\c -> Token (info, CHAR c))   charLiteral
    <|> fmap (\s -> Token (info, STRING s)) stringLiteral
 where
  natural       = Tok.natural lexer'
  float         = Tok.float lexer'
  identifier    = Tok.identifier lexer'
  stringLiteral = Tok.stringLiteral lexer'
  charLiteral   = Tok.charLiteral lexer'
  symbol        = Tok.symbol lexer'
  lparen        = symbol "("
  rparen        = symbol ")"
  lbrack        = symbol "["
  rbrack        = symbol "]"
  lbrace        = symbol "{"
  rbrace        = symbol "}"

lexing :: Stream s m Char => u -> SourceName -> s -> m (Either ParseError [Token])
lexing = runParserT (whiteSpace >> many lexer >>= \toks -> eof >> pure toks)
  where whiteSpace = Tok.whiteSpace lexer'

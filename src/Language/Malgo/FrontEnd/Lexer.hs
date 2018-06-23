{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.Malgo.FrontEnd.Lexer (lex) where

import           Language.Malgo.FrontEnd.Token
import           Language.Malgo.Prelude        hiding (try)

import           Data.String                   (String)
import           Text.Parsec                   (ParseError, ParsecT, SourceName,
                                                Stream, alphaNum, char, eof,
                                                getPosition, letter, oneOf,
                                                runParserT, sourceColumn,
                                                sourceLine, sourceName, try)
import qualified Text.Parsec.Token             as Tok

getInfo :: Monad m => ParsecT s u m Info
getInfo = do
  pos <- getPosition
  return (Info (toS $ sourceName pos, sourceLine pos, sourceColumn pos))

tokenParser :: Stream s m Char => Tok.GenTokenParser s u m
tokenParser = Tok.makeTokenParser
  Tok.LanguageDef
  { Tok.commentStart   = "{-"
  , Tok.commentEnd     = "-}"
  , Tok.commentLine    = "--"
  , Tok.nestedComments = True
  , Tok.identStart     = letter <|> char '_'
  , Tok.identLetter    = alphaNum <|> oneOf "_'"
  , Tok.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.caseSensitive  = True
  , Tok.reservedOpNames =
      [ ".", "+.", "-.", "*.", "/.", ":", "=", "+", "-", "*", "/", "%", "->", ";"
      , "==", "/=", "<", ">", "<=", ">=", "&&", "||" ]
  , Tok.reservedNames =
      [ "let", "type", "rec", "and", "extern", "fn", "if", "else" ]
  }

keyword :: Stream s m Char => Info -> String -> Tag -> ParsecT s u m Token
keyword info word tag = reserved word >> return (Token info tag)
  where reserved = Tok.reserved tokenParser

op :: Stream s m Char => Info -> String -> Tag -> ParsecT s u m Token
op info sym tag = reservedOp sym >> return (Token info tag)
  where reservedOp = Tok.reservedOp tokenParser

token :: Stream s m Char => ParsecT s u m Token
token = do
  info <- getInfo
  foldl (\b (word, tag) -> b <|> keyword info word tag)
    (keyword info "let" LET)
    [ ("type", TYPE), ("rec", REC), ("and", AND)
    , ("extern", EXTERN), ("fn", FN), ("if", IF)
    , ("else", ELSE) ]
    <|> (lparen >> return (Token info LPAREN))
    <|> (rparen >> return (Token info RPAREN))
    <|> (lbrack >> return (Token info LBRACK))
    <|> (rbrack >> return (Token info RBRACK))
    <|> (lbrace >> return (Token info LBRACE))
    <|> (rbrace >> return (Token info RBRACE))
    <|> foldl (\b (sym, tag) -> b <|> op info sym tag)
          (op info "." DOT)
          [ ("+.", PLUS_DOT), ("-.", MINUS_DOT), ("*.", ASTERISK_DOT)
          , ("/.", SLASH_DOT), (":", COLON), ("=", EQUAL)
          , ("+", PLUS), ("-", MINUS), ("*", ASTERISK)
          , ("/", SLASH), ("%", PERCENT), ("->", ARROW)
          , (";", SEMICOLON), ("==", EQ_OP), ("/=", NEQ_OP)
          , ("<", LT_OP), (">", GT_OP), ("<=", LE_OP)
          , (">=", GE_OP), ("&&", AND_OP), ("||", OR_OP) ]
    <|> (Token info . ID . toS <$> identifier)
    <|> try (Token info . FLOAT <$> float)
    <|> (Token info . INT <$> natural)
    <|> (Token info . CHAR <$> charLiteral)
    <|> (Token info . STRING . toS <$> stringLiteral)
  where
    natural = Tok.natural tokenParser
    float = Tok.float tokenParser
    identifier = Tok.identifier tokenParser
    symbol = Tok.symbol tokenParser
    charLiteral = Tok.charLiteral tokenParser
    stringLiteral = Tok.stringLiteral tokenParser
    lparen = symbol "("
    rparen = symbol ")"
    lbrack = symbol "["
    rbrack = symbol "]"
    lbrace = symbol "{"
    rbrace = symbol "}"

lex :: Stream s m Char => u -> SourceName -> s -> m (Either ParseError [Token])
lex = runParserT $ do
  whiteSpace
  toks <- many token
  eof
  return toks
  where whiteSpace = Tok.whiteSpace tokenParser

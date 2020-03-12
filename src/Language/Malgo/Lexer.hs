{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
module Language.Malgo.Lexer
  ( tokenize
  )
where

import           Language.Malgo.Token
import           Language.Malgo.Prelude  hiding ( EQ
                                                , LT
                                                , GT
                                                )

import           Text.Parsec             hiding ( many
                                                , (<|>)
                                                )
import           Text.Parsec.Pos                ( )
import qualified Text.Parsec.Token             as Tok

tokenize :: Stream s m Char => u -> SourceName -> s -> m (Either ParseError [Token])
tokenize =
  runParserT (Tok.whiteSpace tokenParser >> many (getPosition >>= lexer) >>= \toks -> eof >> pure toks)

tokenParser :: Stream s m Char => Tok.GenTokenParser s u m
tokenParser = Tok.makeTokenParser Tok.LanguageDef { Tok.nestedComments  = True
                                                  , Tok.opStart         = oneOf ".+-*/:=%;<&|>"
                                                  , Tok.opLetter        = oneOf ".+-*/:=%;<&|>"
                                                  , Tok.caseSensitive   = True
                                                  , Tok.commentStart    = "{-"
                                                  , Tok.commentEnd      = "-}"
                                                  , Tok.commentLine     = "--"
                                                  , Tok.identStart      = letter <|> oneOf "!?@_#"
                                                  , Tok.identLetter     = alphaNum <|> oneOf "!?@_"
                                                  , Tok.reservedOpNames = map fst reservedOpNames
                                                  , Tok.reservedNames   = map fst reservedNames
                                                  }

reservedNames :: [(String, Tag)]
reservedNames =
  [ ("let"   , LET)
  , ("in"    , IN)
  , ("val"   , VAL)
  , ("fun"   , FUN)
  , ("type"  , TYPE)
  , ("extern", EXTERN)
  , ("and", AND)
  , ("fn"    , FN)
  , ("if"    , IF)
  , ("then"  , THEN)
  , ("else"  , ELSE)
  , ("true"  , BOOL True)
  , ("false" , BOOL False)
  , ("array" , ARRAY)
  , ("match" , MATCH)
  , ("with"  , WITH)
  , ("Int"   , TY_INT)
  , ("Float" , TY_FLOAT)
  , ("Bool"  , TY_BOOL)
  , ("Char"  , TY_CHAR)
  , ("String", TY_STRING)
  ]

reservedOpNames :: [(String, Tag)]
reservedOpNames =
  [ (":=", COLON_EQUAL)
  , (":" , COLON)
  , (";" , SEMICOLON)
  , ("," , COMMA)
  , ("==", EQUAL_EQUAL)
  , ("=" , EQUAL)
  , ("/=", NEQ)
  , ("<" , LT)
  , (">" , GT)
  , ("<=", LE)
  , (">=", GE)
  , ("." , DOT)
  , ("+.", PLUS_DOT)
  , ("-.", MINUS_DOT)
  , ("*.", ASTERISK_DOT)
  , ("/.", SLASH_DOT)
  , ("%" , PERCENT)
  , ("+" , PLUS)
  , ("-" , MINUS)
  , ("*" , ASTERISK)
  , ("/" , SLASH)
  , ("&&", AND_AND)
  , ("||", OR_OR)
  , ("->", ARROW)
  , ("<-", LARROW)
  , ("|" , BAR)
  , ("=>", DARROW)
  ]

lexer :: Stream s m Char => SourcePos -> ParsecT s u m Token
lexer pos =
  asumMap (simpleToken Tok.reserved) reservedNames
    <|> asumMap (simpleToken Tok.reservedOp) reservedOpNames
    <|> asumMap
          (simpleToken (void <<$>> Tok.symbol))
          [("(", LPAREN), (")", RPAREN), ("[", LBRACK), ("]", RBRACK), ("{", LBRACE), ("}", RBRACE)]
    <|> try (valueToken Tok.float FLOAT)
    <|> valueToken Tok.natural       INT
    <|> valueToken (\t -> lookAhead upper >> Tok.identifier t) LID
    <|> valueToken Tok.identifier    ID
    <|> valueToken Tok.charLiteral   CHAR
    <|> valueToken Tok.stringLiteral STRING
 where
  simpleToken eatToken (s, t) = eatToken tokenParser s >> pure (Token (pos, t))
  valueToken scanToken builder = scanToken tokenParser >>= \v -> pure (Token (pos, builder v))

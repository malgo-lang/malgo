{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Language.Malgo.Token
  ( Token(..)
  , Tag(..)
  , _info
  , _tag
  )
where

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.Prelude

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
    | BAR
    | DARROW
    | MATCH
    | WITH
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
    deriving stock (Eq, Show)

newtype Token =
    Token (Info, Tag)
    deriving stock (Eq, Show)

_info :: Token -> Info
_info (Token a) = fst a

_tag :: Token -> Tag
_tag (Token a) = snd a

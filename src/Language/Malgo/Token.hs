{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Language.Malgo.Token
  ( Token(..)
  , Tag(..)
  , _info
  , _tag
  )
where

import           Language.Malgo.Prelude
import           Language.Malgo.Pretty

import           Language.Malgo.FrontEnd.Info

import qualified Text.PrettyPrint.HughesPJClass
                                               as P

data Tag
    = LET
    | IN
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
    | COLON_EQUAL
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
    | AS
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

instance Pretty Token where
  pPrint (Token (i, t)) = pPrint i P.$+$ pPrint t

instance Pretty Tag where
  pPrint LET                     = "let"
  pPrint IN                      = "in"
  pPrint VAL                     = "val"
  pPrint FUN                     = "fun"
  pPrint TYPE                    = "type"
  pPrint EXTERN                  = "extern"
  pPrint LPAREN                  = "("
  pPrint RPAREN                  = ")"
  pPrint LBRACK                  = "["
  pPrint RBRACK                  = "]"
  pPrint LBRACE                  = "{"
  pPrint RBRACE                  = "}"
  pPrint COMMA                   = ","
  pPrint COLON                   = ":"
  pPrint SEMICOLON               = ";"
  pPrint EQUAL                   = "="
  pPrint COLON_EQUAL             = ":="
  pPrint FN                      = "fn"
  pPrint IF                      = "if"
  pPrint THEN                    = "then"
  pPrint ELSE                    = "else"
  pPrint DOT                     = "."
  pPrint PLUS                    = "+"
  pPrint PLUS_DOT                = "+."
  pPrint MINUS                   = "-"
  pPrint MINUS_DOT               = "-."
  pPrint ASTERISK                = "*"
  pPrint ASTERISK_DOT            = "*."
  pPrint SLASH                   = "/"
  pPrint SLASH_DOT               = "/."
  pPrint PERCENT                 = "%"
  pPrint ARROW                   = "->"
  pPrint Language.Malgo.Token.EQ = "=="
  pPrint NEQ                     = "/="
  pPrint Language.Malgo.Token.LT = "<"
  pPrint Language.Malgo.Token.GT = ">"
  pPrint LE                      = "<="
  pPrint GE                      = ">="
  pPrint AND                     = "&&"
  pPrint OR                      = "||"
  pPrint ARRAY                   = "array"
  pPrint LARROW                  = "<-"
  pPrint BAR                     = "|"
  pPrint DARROW                  = "=>"
  pPrint MATCH                   = "match"
  pPrint WITH                    = "with"
  pPrint AS                      = "as"
  pPrint (ID     x)              = P.text x
  pPrint (INT    x)              = P.integer x
  pPrint (FLOAT  x)              = P.double x
  pPrint (BOOL   x)              = pPrint x
  pPrint (CHAR   x)              = pPrint x
  pPrint (STRING x)              = pPrint x
  pPrint TY_INT                  = "Int"
  pPrint TY_FLOAT                = "Float"
  pPrint TY_BOOL                 = "Bool"
  pPrint TY_CHAR                 = "Char"
  pPrint TY_STRING               = "String"

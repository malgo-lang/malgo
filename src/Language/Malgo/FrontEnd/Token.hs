{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Malgo.FrontEnd.Token where

import           Language.Malgo.Prelude

data Tag
  = LET
  | TYPE
  | REC
  | AND
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
  | FN
  | IF
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
  | EQ_OP
  | NEQ_OP
  | LT_OP
  | GT_OP
  | LE_OP
  | GE_OP
  | AND_OP
  | OR_OP
  | ID { _id :: Name }
  | INT { _int :: Integer }
  | FLOAT { _float :: Double }
  | BOOL { _bool :: Bool }
  | CHAR { _char :: Char }
  | STRING { _str :: Text }
  deriving (Eq, Show)

data Token = Token { _tokenInfo :: Info, _tokenTag :: Tag }
  deriving (Eq, Show)

makeLenses ''Token

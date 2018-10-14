{-# LANGUAGE StrictData        #-}
module Language.Malgo.FrontEnd.Token where

import           Language.Malgo.FrontEnd.Loc
import           Universum

data Tag
  = LET
  | IN
  | DATA
  | TYPE
  | REC
  | AND
  | TRUE
  | FALSE
  | FORALL
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
  | CASE
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
  | FN
  | ARROW
  | DARROW
  | EQ_OP
  | NEQ_OP
  | LT_OP
  | GT_OP
  | LE_OP
  | GE_OP
  | AND_OP
  | OR_OP
  | ID { _id :: Text }
  | LID { _id :: Text }
  | INT { _int :: Integer }
  | FLOAT { _float :: Double }
  | BOOL { _bool :: Bool }
  | CHAR { _char :: Char }
  | STRING { _str :: Text }
  deriving (Eq, Show)

type Token = Loc Tag

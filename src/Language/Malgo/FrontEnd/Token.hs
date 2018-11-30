{-# LANGUAGE StrictData #-}
module Language.Malgo.FrontEnd.Token where

import           Data.Text                   (Text)
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.Pretty

data Tag
  = LET
  | IN
  | TYPE
  | REC
  | IF
  | THEN
  | ELSE
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
  | DOT
  | PLUS
  | PLUS_DOT
  | MINUS
  | MINUS_DOT
  | ASTERISK
  | ASTERISK_DOT
  | SLASH
  | SLASH_DOT
  | BACKSLASH
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
  | ID { _id :: Text }
  | LID { _id :: Text }
  | INT { _int :: Integer }
  | FLOAT { _float :: Double }
  | CHAR { _char :: Char }
  | STRING { _str :: Text }
  deriving (Eq, Show)

instance Pretty Tag where
  pPrint = text . show

type Token = Loc Tag

-- -*- mode: text -*-
{
{-# LANGUAGE NoStrictData #-}
module Language.Malgo.FrontEnd.Parser where

import Prelude
import Data.Text (Text)
import Language.Malgo.IR.AST
import Language.Malgo.FrontEnd.Loc
import Language.Malgo.FrontEnd.Token
}

%name parse
%tokentype { Token }
%error { parseError }
%errorhandlertype explist

%token
LET { Loc _ LET }
TYPE { Loc _ TYPE }
REC { Loc _ REC }
AND { Loc _ AND }
CASE { Loc _ CASE }
FN { Loc _ FN }
TRUE { Loc _ TRUE }
FALSE { Loc _ FALSE }
'(' { Loc _ LPAREN }
')' { Loc _ RPAREN }
'[' { Loc _ LBRACK }
']' { Loc _ RBRACK }
'{' { Loc _ LBRACE }
'}' { Loc _ RBRACE }
'.' { Loc _ DOT }
"+." { Loc _ PLUS_DOT }
"-." { Loc _ MINUS_DOT }
"*." { Loc _ ASTERISK_DOT }
"/." { Loc _ SLASH_DOT }
':' { Loc _ COLON }
'=' { Loc _ EQUAL }
'+' { Loc _ PLUS }
'-' { Loc _ MINUS }
'*' { Loc _ ASTERISK }
'/' { Loc _ SLASH }
'%' { Loc _ PERCENT }
"->" { Loc _ ARROW }
"=>" { Loc _ DARROW }
';' { Loc _ SEMICOLON }
"==" { Loc _ EQ_OP }
"/=" { Loc _ NEQ_OP }
'<' { Loc _ LT_OP }
'>' { Loc _ GT_OP }
"<=" { Loc _ LE_OP }
">=" { Loc _ GE_OP }
'&' { Loc _ AND_OP }
'|' { Loc _ OR_OP }
',' { Loc _ COMMA }
ID { Loc _ (ID _) }
FLOAT { Loc _ (FLOAT _) }
INT { Loc _ (INT _) }
CHAR { Loc _ (CHAR _) }
STRING { Loc _ (STRING _) }

%left App

%%

program : decs {}

decs : decs dec {}
     |          {}

dec : scdec {}

scdec : ID params '=' expr {}

params : params ID {}
       |           {}

expr :: { Expr Text }
expr : aexpr { $1 }

aexpr : ID { Var (srcSpan $1) (_id $ unLoc $1) }
      | INT { Literal (srcSpan $1) (Int (_int $ unLoc $1)) }
      | FLOAT { Literal (srcSpan $1) (Float (_float $ unLoc $1)) }
      | TRUE { Literal (srcSpan $1) (Bool True) }
      | FALSE { Literal (srcSpan $1) (Bool False) }
      | CHAR { Literal (srcSpan $1) (Char (_char $ unLoc $1)) }
      | STRING { error "string literal is not supported" }
      | '{' field_exprs '}' { Record (srcSpan ($1, $3)) $2 }


field_expr :: { (Text, Expr Text) }
field_expr : ID '=' expr { (_id $ unLoc $1, $3) }

field_exprs :: { [(Text, Expr Text)] }
field_exprs : field_exprs_rev { reverse $1 }
field_exprs_rev : field_exprs_rev ',' field_expr { $3 : $1 }
                | { [] }

{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ "Parse error at EOF: " <> show xs <> " are expected."
parseError (t:_, xs) = error $ "Parse error: " <> show t <> " is got, but " <> show xs <> "are expected."
}

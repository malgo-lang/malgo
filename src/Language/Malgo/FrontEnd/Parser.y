-- -*- mode: fundamental -*-
{
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.Parser where

import Language.Malgo.FrontEnd.Token
import Language.Malgo.FrontEnd.Loc
import Language.Malgo.IR.Core
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
EXTERN { Loc _ EXTERN }
IF { Loc _ IF }
ELSE { Loc _ ELSE }
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
'+.' { Loc _ PLUS_DOT }
'-.' { Loc _ MINUS_DOT }
'*.' { Loc _ ASTERISK_DOT }
'/.' { Loc _ SLASH_DOT }
':' { Loc _ COLON }
'=' { Loc _ EQUAL }
'+' { Loc _ PLUS }
'-' { Loc _ MINUS }
'*' { Loc _ ASTERISK }
'/' { Loc _ SLASH }
'%' { Loc _ PERCENT }
'->' { Loc _ ARROW }
';' { Loc _ SEMICOLON }
'==' { Loc _ EQ_OP }
'/=' { Loc _ NEQ_OP }
'<' { Loc _ LT_OP }
'>' { Loc _ GT_OP }
'<=' { Loc _ LE_OP }
'>=' { Loc _ GE_OP }
'&&' { Loc _ AND_OP }
'||' { Loc _ OR_OP }
ID { Loc _ ID{} }
INT { Loc _ INT{} }
FLOAT { Loc _ FLOAT{} }
CHAR { Loc _ CHAR{} }
STRING { Loc _ STRING {} }

%right '->'
%right prec_if
%nonassoc '==' '<>'
%nonassoc '<' '>' '<=' '>='
%left '&&' '||'
%left '+' '-' '+.' '-.'
%left '*' '/' '%' '*.' '/.'
%left '.'
%nonassoc NEG
%left APP

%%

exp: simple_exp { $1 }
   | '-' INT %prec NEG { Int (srcSpan ($1, $2)) (0 - _int (unLoc $2)) }
   | '-' FLOAT %prec NEG { Float (srcSpan ($1, $2)) (0 - _float (unLoc $2)) }
   | simple_exp simple_exp %prec APP { App (srcSpan ($1, $2)) $1 $2 }

simple_exp: ID { Var (srcSpan $1) (_id $ unLoc $1) }
          | INT { Int (srcSpan $1) (_int $ unLoc $1) }
          | FLOAT { Float (srcSpan $1) (_float $ unLoc $1) }
          | TRUE { Bool (srcSpan $1) True }
          | FALSE { Bool (srcSpan $1) False }
          | CHAR { Char (srcSpan $1) (_char $ unLoc $1) }
          | STRING { String (srcSpan $1) (_str $ unLoc $1) }
          | '(' exp ')' { $2 }

{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ "Parse error at EOF: " <> show xs <> " are expected."
parseError (t:_, xs) = error $ "Parse error: " <> show t <> " is got, but " <> show xs <> " are expected."
}

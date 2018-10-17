-- -*- mode: text -*-
{
{-# LANGUAGE NoStrictData, OverloadedStrings #-}
module Language.Malgo.FrontEnd.Parser (parse) where

import Prelude
import Data.Text (Text)
import Language.Malgo.IR.AST
import Language.Malgo.FrontEnd.Loc
import Language.Malgo.FrontEnd.Token
}

%name parse decs

%tokentype { Token }
%error { parseError }
%errorhandlertype explist

%token
LET { Loc _ LET }
IN { Loc _ IN }
TYPE { Loc _ TYPE }
REC { Loc _ REC }
IF { Loc _ IF }
THEN { Loc _ THEN }
ELSE { Loc _ ELSE }
TRUE { Loc _ TRUE }
FALSE { Loc _ FALSE }
FORALL { Loc _ FORALL }
'(' { Loc _ LPAREN }
')' { Loc _ RPAREN }
'[' { Loc _ LBRACK }
']' { Loc _ RBRACK }
'{' { Loc _ LBRACE }
'}' { Loc _ RBRACE }
',' { Loc _ COMMA }
':' { Loc _ COLON }
';' { Loc _ SEMICOLON }
'=' { Loc _ EQUAL }
'.' { Loc _ DOT }
'+' { Loc _ PLUS }
"+." { Loc _ PLUS_DOT }
'-' { Loc _ MINUS }
"-." { Loc _ MINUS_DOT }
'*' { Loc _ ASTERISK }
"*." { Loc _ ASTERISK_DOT }
'/' { Loc _ SLASH }
"/." { Loc _ SLASH_DOT }
'%' { Loc _ PERCENT }
"->" { Loc _ ARROW }
"==" { Loc _ EQ_OP }
"/=" { Loc _ NEQ_OP }
'<' { Loc _ LT_OP }
'>' { Loc _ GT_OP }
"<=" { Loc _ LE_OP }
">=" { Loc _ GE_OP }
'&' { Loc _ AND_OP }
'|' { Loc _ OR_OP }
ID { Loc _ (ID _) }
LID { Loc _ (LID _) }
INT { Loc _ (INT _) }
FLOAT { Loc _ (FLOAT _) }
CHAR { Loc _ (CHAR _) }
STRING { Loc _ (STRING _) }

%right "->"
%right IN THEN ELSE
%nonassoc "==" "/="
%nonassoc '<' '>' "<=" ">="
%left '+' '-' "+." "-." '&'
%left '*' '/' "*." "/." '%' '|'
%left '.'
%left prec_app

%%

decs :: { [Decl Text] }
decs : { [] }

{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ "Parse error at EOF: " <> show xs <> " are expected."
parseError (t:_, xs) = error $ "Parse error: " <> show t <> " is got, but " <> show xs <> "are expected."
}

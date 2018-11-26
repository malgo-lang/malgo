-- -*- mode: text -*-
{
{-# LANGUAGE NoStrictData, OverloadedStrings #-}
module Language.Malgo.FrontEnd.Parser (parse) where

import Prelude
import Data.Text (Text)
import Language.Malgo.Pretty
import Language.Malgo.IR.AST
import Language.Malgo.FrontEnd.Loc
import Language.Malgo.FrontEnd.Token
import Language.Malgo.Type
}

%name parse program

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
'\\' { Loc _ BACKSLASH }
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
%right prec_fn
%nonassoc "==" "/="
%nonassoc '<' '>' "<=" ">="
%left '+' '-' "+." "-." '&'
%left '*' '/' "*." "/." '%' '|'
%right prec_negate
%left '.'
%left prec_app

%%

program :: { Program Text }
program : decs { Program $1 }

decs :: { [Decl Text] }
decs : decs_rev { reverse $1 }
decs_rev : decs_rev dec { $2 : $1 }
         | { [] }

dec : ID id_list '=' expr ';' { ScDef (srcSpan ($1, $5)) (_id $ unLoc $1) $2 $4 }
    | ID ':' typescheme ';' { ScAnn (srcSpan ($1, $4)) (_id $ unLoc $1) $3 }
    | TYPE LID id_list '=' type ';' { TypeDef (srcSpan ($1, $6)) (_id $ unLoc $2) $3 $5 }

id_list : id_list_rev { reverse $1 }
id_list_rev : { [] }
            | id_list_rev ID { _id (unLoc $2) : $1 }


aexpr : ID { Var (srcSpan $1) (_id $ unLoc $1) }
      | INT { Literal (srcSpan $1) (Int $ _int $ unLoc $ $1) }
      | FLOAT { Literal (srcSpan $1) (Float $ _float $ unLoc $1) }
      | TRUE { Literal (srcSpan $1) (Bool True) }
      | FALSE { Literal (srcSpan $1) (Bool False) }
      | CHAR { Literal (srcSpan $1) (Char $ _char $ unLoc $1) }
      | STRING { Literal (srcSpan $1) (String $ _str $ unLoc $1) }
      | '(' ')' { Tuple (srcSpan ($1, $2)) [] }
      | '(' expr ')' { $2 }
      | '(' expr_list_comma ')' { Tuple (srcSpan ($1, $3)) $2 }

expr : aexpr { $1 }
     | '-' INT %prec prec_negate { Literal (srcSpan $1) (Int $ negate $ _int $ unLoc $2) }
     | '-' FLOAT %prec prec_negate { Literal (srcSpan $1) (Float $ negate $ _float $ unLoc $2) }
     | expr '+' expr { BinOp (srcSpan ($1, $3)) Add $1 $3 }
     | expr '-' expr { BinOp (srcSpan ($1, $3)) Sub $1 $3 }
     | expr '*' expr { BinOp (srcSpan ($1, $3)) Mul $1 $3 }
     | expr '/' expr { BinOp (srcSpan ($1, $3)) Div $1 $3 }
     | expr '%' expr { BinOp (srcSpan ($1, $3)) Mod $1 $3 }
     | expr "+." expr { BinOp (srcSpan ($1, $3)) FAdd $1 $3 }
     | expr "-." expr { BinOp (srcSpan ($1, $3)) FSub $1 $3 }
     | expr "*." expr { BinOp (srcSpan ($1, $3)) FMul $1 $3 }
     | expr "/." expr { BinOp (srcSpan ($1, $3)) FDiv $1 $3 }
     | expr "==" expr { BinOp (srcSpan ($1, $3)) Eq $1 $3 }
     | expr "/=" expr { BinOp (srcSpan ($1, $3)) Neq $1 $3 }
     | expr '<' expr { BinOp (srcSpan ($1, $3)) Lt $1 $3 }
     | expr '>' expr { BinOp (srcSpan ($1, $3)) Gt $1 $3 }
     | expr "<=" expr { BinOp (srcSpan ($1, $3)) Le $1 $3 }
     | expr ">=" expr { BinOp (srcSpan ($1, $3)) Ge $1 $3 }
     | expr '&' expr { BinOp (srcSpan ($1, $3)) And $1 $3 }
     | expr '|' expr { BinOp (srcSpan ($1, $3)) Or $1 $3 }
     | IF expr THEN expr ELSE expr { If (srcSpan ($1, $6)) $2 $4 $6 }
     | LET bind IN expr { Let (srcSpan ($1, $4)) $2 $4 }
     | app { $1 }
     | fn { $1 }
     | app fn { Apply (srcSpan ($1, $2)) $1 $2 }

fn : '\\' id_list "->" expr %prec prec_fn { Fn (srcSpan ($1, $4)) $2 $4 }

expr_list_comma : expr_list_comma_rev { reverse $1 }
expr_list_comma_rev : expr ',' expr { [$3, $1] }
                    | expr_list_comma_rev ',' expr { $3 : $1 }

app : aexpr aexpr %prec prec_app { Apply (srcSpan ($1, $2)) $1 $2 }
    | app aexpr %prec prec_app { Apply (srcSpan ($1, $2)) $1 $2 }

bind : ID '=' expr { NonRec (srcSpan ($1, $3)) (_id $ unLoc $1) Nothing $3 }
     | ID ':' typescheme '=' expr { NonRec (srcSpan ($1, $5)) (_id $ unLoc $1) (Just $3) $5 }
     | REC ID fn_params '=' expr { Rec (srcSpan ($1, $5)) (_id $ unLoc $2) $3 Nothing $5 }
     | REC ID fn_params ':' typescheme '=' expr { Rec (srcSpan ($1, $7)) (_id $ unLoc $2) $3 (Just $5) $7 }
     | '(' id_list_comma ')' '=' expr { TuplePat (srcSpan ($1, $5)) $2 Nothing $5 }
     | '(' id_list_comma ')' ':' typescheme '=' expr { TuplePat (srcSpan ($1, $7)) $2 (Just $5) $7 }

id_list_comma : id_list_comma_rev { reverse $1 }
id_list_comma_rev : ID ',' ID { [_id $ unLoc $3, _id $ unLoc $1] }
                  | id_list_comma_rev ',' ID { _id (unLoc $3) : $1 }

fn_params : fn_params_rev { reverse $1 }
fn_params_rev : { [] }
              | fn_params_rev ID { _id (unLoc $2) : $1 }

typescheme : FORALL id_list '.' type { Forall $2 $4 }
           | type { Forall [] $1 }

atype : LID { TyApp (SimpleC $ _id $ unLoc $1) [] }
      | ID { TyVar $ _id $ unLoc $1 }
      | '(' type_list_comma ')' { TyApp (PrimC $ TupleC (length $2)) $2 }
      | '(' type ')' { $2 }
      | '(' ')' { TyApp (PrimC $ TupleC 0) [] }

type : atype { $1 }
     | LID type_list { TyApp (SimpleC $ _id $ unLoc $1) $2 }
     | type "->" type { TyApp (PrimC ArrowC) [$1, $3] }

type_list : type_list_rev { reverse $1 }
type_list_rev : atype { [$1] }
              | type_list_rev atype { $2 : $1 }

type_list_comma : type_list_comma_rev { reverse $1 }
type_list_comma_rev : type ',' type { [$3, $1] }
                    | type_list_comma_rev ',' type { $3 : $1 }

{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ show $ "Parse error at EOF: " <> pPrint xs <> " are expected."
parseError (t:_, xs) = error $ show $ "Parse error: " <> pPrint t <> " is got, but " <> pPrint xs <> " are expected."
}

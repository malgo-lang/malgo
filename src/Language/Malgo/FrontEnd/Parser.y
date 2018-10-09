-- -*- mode: text -*-
{
{-# LANGUAGE NoStrictData, OverloadedStrings #-}
module Language.Malgo.FrontEnd.Parser where

import Prelude
import Data.Text (Text)
import Language.Malgo.IR.AST
import Language.Malgo.FrontEnd.Loc
import Language.Malgo.FrontEnd.Token
}

%name parse decs
%name parseExpr expr
%name parseDecl dec
%name parseType type

%tokentype { Token }
%error { parseError }
%errorhandlertype explist

%token
LET { Loc _ LET }
IN { Loc _ IN }
TYPE { Loc _ TYPE }
ALIAS { Loc _ ALIAS }
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
TYCON { Loc _ (TYCON _) }
FLOAT { Loc _ (FLOAT _) }
INT { Loc _ (INT _) }
CHAR { Loc _ (CHAR _) }
STRING { Loc _ (STRING _) }

%left ';'
%left AND
%left TyApp
%right "->"
%left App

%%

decs :: { [Decl Text] }
decs : decs_rev { reverse $1 }
decs_rev : decs_rev ';' dec { $3 : $1 }
         | decs_rev ';' { $1 }
         | dec { [$1] }
         | { [] }

dec :: { Decl Text }
dec : scdec { $1 }
    | scann { $1 }
    | typedec { $1 }

scdec :: { Decl Text }
scdec : ID params '=' expr { ScDef (srcSpan ($1, $4)) (_id $ unLoc $1) $2 $4 }

scann :: { Decl Text }
scann : ID ':' type { ScAnn (srcSpan $1) (_id $ unLoc $1) $3 }

typedec :: { Decl Text }
typedec : TYPE TYCON ty_params '=' type { TypeDef (srcSpan $1) (_tycon $ unLoc $2) $3 $5 }

ty_params :: { [Text] }
ty_params : ty_params_rev { reverse $1 }
ty_params_rev : { [] }
              | ty_params_rev ID { _id (unLoc $2) : $1 }

params :: { [Text] }
params : params_rev { reverse $1 }
params_rev : { [] }
           | params_rev ID { _id (unLoc $2) : $1 }

expr :: { Expr Text }
expr : app { $1 }
     | aexpr { $1 }
     | FN fn_params "->" expr { Fn (srcSpan ($1, $4)) $2 $4 }
     | LET ID ':' type '=' expr IN expr
       { Let (srcSpan ($1, $8))
         (NonRec (srcSpan ($2, $5))
          (_id $ unLoc $2) (Just $4) $6) $8 }
     | LET ID '=' expr IN expr
       { Let (srcSpan ($1, $6))
         (NonRec (srcSpan ($2, $4))
          (_id $ unLoc $2) Nothing $4) $6 }
     | LET REC recbinds IN expr { Let (srcSpan ($1, $5)) $3 $5 }
     | CASE aexpr clauses { Case (srcSpan $1) $2 $3 }

clauses : '{' clauses_rev '}' { reverse $2 }
clauses_rev : clauses_rev '|' clause { $3 : $1 }
            | { [] }
clause : '<' ID '=' ID '>' "=>" expr { VariantPat (srcSpan ($1, $7)) (_id $ unLoc $2) (_id $ unLoc $4) [] $7 }
       | '<' ID '=' ID ',' field_types '>' "=>" expr { VariantPat (srcSpan ($1, $9)) (_id $ unLoc $2) (_id $ unLoc $4) $6 $9 }
       | TRUE "=>" expr { BoolPat (srcSpan ($1, $3)) True $3 }
       | FALSE "=>" expr { BoolPat (srcSpan ($1, $3)) False $3 }
       | ID "=>" expr { VarPat (srcSpan ($1, $3)) (_id $ unLoc $1) $3 }

recbind :: { (SrcSpan, Text, Maybe (SType Text), [Text], Expr Text) }
recbind : ID params ':' type '=' expr
          { ( srcSpan ($1, $6)
            , _id (unLoc $1)
            , Just $4, $2, $6) }
        | ID params '=' expr
          { ( srcSpan ($1, $4)
            , _id (unLoc $1), Nothing, $2, $4) }

recbinds :: { Bind Text }
recbinds : recbinds_rev { Rec $ reverse $1 }
recbinds_rev : recbind { [$1] }
             | recbinds_rev AND recbind { $3 : $1 }

app :: { Expr Text }
app : aexpr aexpr %prec App { Apply (srcSpan ($1, $2)) $1 $2 }
    | app aexpr %prec App { Apply (srcSpan ($1, $2)) $1 $2 }

fn_params :: { [(Text, Maybe (SType Text))] }
fn_params : fn_params_rev { reverse $1 }
fn_params_rev : fn_params_rev '(' ID ':' type ')' { (_id $ unLoc $3, Just $5) : $1 }
              | fn_params_rev ID { (_id $ unLoc $2, Nothing) : $1 }
              | { [] }

aexpr :: { Expr Text }
aexpr : ID { Var (srcSpan $1) (_id $ unLoc $1) }
      | INT { Literal (srcSpan $1) (Int (_int $ unLoc $1)) }
      | FLOAT { Literal (srcSpan $1) (Float (_float $ unLoc $1)) }
      | TRUE { Literal (srcSpan $1) (Bool True) }
      | FALSE { Literal (srcSpan $1) (Bool False) }
      | CHAR { Literal (srcSpan $1) (Char (_char $ unLoc $1)) }
      | STRING { error "string literal is not supported" }
      | aexpr '.' ID { Access (srcSpan ($1, $3)) $1 (_id $ unLoc $3) }
      | '{' field_exprs '}' { Record (srcSpan ($1, $3)) $2 }
      | '<' field_expr '>' { Variant (srcSpan ($1, $3)) (fst $2) (snd $2) [] }
      | '<' field_expr ',' field_types '>' { Variant (srcSpan ($1, $3)) (fst $2) (snd $2) $4 }
      | '(' expr ')' { $2 }

field_expr :: { (Text, Expr Text) }
field_expr : ID '=' expr { (_id $ unLoc $1, $3) }

field_exprs :: { [(Text, Expr Text)] }
field_exprs : field_exprs_rev { reverse $1 }
            | { [] }
field_exprs_rev : field_exprs_rev ',' field_expr { $3 : $1 }
                | field_exprs_rev ',' { $1 }
                | field_expr { [$1] }

type :: { SType Text }
type : tycon ty_args %prec TyApp { STyApp (srcSpan $1) $1 $2 }
     | arrow { $1 }
     | atype { $1 }

arrow :: { SType Text }
arrow : type "->" type { STyApp (srcSpan ($1, $3)) (SimpleC (srcSpan $2) "->") [$1, $3] }

atype :: { SType Text }
atype : ID { STyVar (srcSpan $1) (_id $ unLoc $1) }
      | tycon { STyApp (srcSpan $1) $1 [] }
      | '{' field_types '}' { STyApp (srcSpan ($1, $3)) (SRecordC (srcSpan ($1, $3)) $2) [] }
      | '<' field_types '>' { STyApp (srcSpan ($1, $3)) (SVariantC (srcSpan ($1, $3)) $2) [] }
      | '(' type ')' { $2 }

tycon :: { STyCon Text }
tycon : TYCON { SimpleC (srcSpan $1) (_tycon $ unLoc $1) }

ty_args :: { [SType Text] }
ty_args : ty_args_rev { reverse $1 }
ty_args_rev : ty_args_rev atype { $2 : $1 }
            | atype { [$1] }

field_type :: { (Text, SType Text) }
field_type : ID ':' type { (_id $ unLoc $1, $3) }

field_types :: { [(Text, SType Text)] }
field_types : field_types_rev { reverse $1 }
            | { [] }
field_types_rev : field_types_rev ',' field_type { $3 : $1 }
                | field_types_rev ',' { $1 }
                | field_type { [$1] }

{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ "Parse error at EOF: " <> show xs <> " are expected."
parseError (t:_, xs) = error $ "Parse error: " <> show t <> " is got, but " <> show xs <> "are expected."
}

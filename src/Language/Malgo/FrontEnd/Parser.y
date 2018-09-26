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

%left AND
%right "->"
%nonassoc ':'
%nonassoc '='
%left App

%%

decs :: { [Decl Text] }
decs : decs_rev { reverse $1 }
decs_rev : decs dec { $2 : $1 }
         | { [] }

dec :: { Decl Text }
dec : scdec { $1 }
    | scann { $1 }
    | aliasdec { $1 }
    | typedec { $1 }

scdec :: { Decl Text }
scdec : ID params '=' expr { ScDef (srcSpan ($1, $4)) (_id $ unLoc $1) $2 $4 }

scann : ID ':' type { ScAnn (srcSpan $1) (_id $ unLoc $1) $3 }

aliasdec : ALIAS TYCON ty_params '=' type { AliasDef (srcSpan $1) (_tycon $ unLoc $2) $3 $5 }

typedec : TYPE TYCON ty_params '=' type { TypeDef (srcSpan $1) (_tycon $ unLoc $2) $3 $5 }

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

recbind : ID params ':' type '=' expr
          { ( srcSpan ($1, $6)
            , _id (unLoc $1)
            , Just $4, $2, $6) }
        | ID params '=' expr
          { ( srcSpan ($1, $4)
            , _id (unLoc $1), Nothing, $2, $4) }

recbinds : recbinds_rev { Rec $ reverse $1 }
recbinds_rev : recbind { [$1] }
             | recbinds_rev AND recbind { $3 : $1 }

app : aexpr aexpr %prec App { Apply (srcSpan ($1, $2)) $1 $2 }
    | app aexpr %prec App { Apply (srcSpan ($1, $2)) $1 $2 }

fn_params : fn_params_rev { reverse $1 }
fn_params_rev : fn_params_rev '(' ID ':' type ')' { (_id $ unLoc $3, Just $5) : $1 }
              | fn_params_rev ID { (_id $ unLoc $2, Nothing) : $1 }
              | { [] }

aexpr : ID { Var (srcSpan $1) (_id $ unLoc $1) }
      | INT { Literal (srcSpan $1) (Int (_int $ unLoc $1)) }
      | FLOAT { Literal (srcSpan $1) (Float (_float $ unLoc $1)) }
      | TRUE { Literal (srcSpan $1) (Bool True) }
      | FALSE { Literal (srcSpan $1) (Bool False) }
      | CHAR { Literal (srcSpan $1) (Char (_char $ unLoc $1)) }
      | STRING { error "string literal is not supported" }
      | '{' field_exprs '}' { Record (srcSpan ($1, $3)) $2 }
      | '<' field_expr '>' ':' type { Variant (srcSpan ($1, $3)) (fst $2) (snd $2) $5 }
      | '(' expr ')' { $2 }

field_expr :: { (Text, Expr Text) }
field_expr : ID '=' expr { (_id $ unLoc $1, $3) }

field_exprs :: { [(Text, Expr Text)] }
field_exprs : field_exprs_rev { reverse $1 }
field_exprs_rev : field_exprs_rev ',' field_expr { $3 : $1 }
                | field_expr { [$1] }
                | { [] }

type : tycon ty_args { STyApp $1 $2 }
     | atype { $1 }

atype : ID { STyVar (_id $ unLoc $1) }
      | tycon { STyApp $1 [] }
      | '(' type ')' { $2 }

tycon : TYCON { SimpleC (_tycon $ unLoc $1) }
      | '{' field_types '}' { SRecordC $2 }

ty_args : ty_args_rev { reverse $1 }
ty_args_rev : ty_args_rev atype { $2 : $1 }
            | atype { [$1] }

field_type : ID ':' type { (_id $ unLoc $1, $3) }

field_types : field_types_rev { reverse $1 }
field_types_rev : field_types_rev ',' field_type { $3 : $1 }
                | field_type { [$1] }
                | { [] }

{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ "Parse error at EOF: " <> show xs <> " are expected."
parseError (t:_, xs) = error $ "Parse error: " <> show t <> " is got, but " <> show xs <> "are expected."
}

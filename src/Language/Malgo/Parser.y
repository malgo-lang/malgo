-- -*- mode: fundamental -*-
{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE NoStrict #-}
module Language.Malgo.Parser where

import Prelude hiding (EQ, LT, GT)
import Language.Malgo.Lexer
import Language.Malgo.TypeRep.Type
import Language.Malgo.IR.Syntax
import Data.String
}

%name parse
%tokentype { Token }
%error { parseError }
%errorhandlertype explist

%token
let   { Token (_, LET) }
in    { Token (_, IN) }
end   { Token (_, END) }
val   { Token (_, VAL) }
fun   { Token (_, FUN) }
type  { Token (_, TYPE) }
extern { Token (_, EXTERN) }
fn { Token (_, FN) }
if    { Token (_, IF) }
then  { Token (_, THEN) }
else  { Token (_, ELSE) }
array { Token (_, ARRAY) }
Int   { Token (_, TY_INT) }
Float { Token (_, TY_FLOAT) }
Bool  { Token (_, TY_BOOL) }
Char  { Token (_, TY_CHAR) }
String { Token (_, TY_STRING) }
'('   { Token (_, LPAREN) }
')'   { Token (_, RPAREN) }
'['   { Token (_, LBRACK) }
']'   { Token (_, RBRACK) }
'{'   { Token (_, LBRACE) }
'}'   { Token (_, RBRACE) }
':='  { Token (_, SEMICOLON_EQUAL) }
':'   { Token (_, COLON) }
';'   { Token (_, SEMICOLON) }
','   { Token (_, COMMA) }
'=='  { Token (_, EQ) }
'='   { Token (_, EQUAL) }
'<>'  { Token (_, NEQ) }
'<'   { Token (_, LT) }
'>'   { Token (_, GT) }
'<='  { Token (_, LE) }
'>='  { Token (_, GE) }
'.'   { Token (_, DOT) }
'+'   { Token (_, PLUS) }
'-'   { Token (_, MINUS) }
'*'   { Token (_, ASTERISK) }
'/'   { Token (_, SLASH) }
'+.'   { Token (_, PLUS_DOT) }
'-.'   { Token (_, MINUS_DOT) }
'*.'   { Token (_, ASTERISK_DOT) }
'/.'   { Token (_, SLASH_DOT) }
'%'   { Token (_, PERCENT) }
'&&'  { Token (_, AND) }
'||'  { Token (_, OR) }
'->'  { Token (_, ARROW) }
'<-'  { Token (_, LARROW) }
id    { Token (_, ID _) }
float { Token (_, FLOAT _) }
int   { Token (_, INT _) }
char  { Token (_, CHAR _) }
bool  { Token (_, BOOL _) }
str   { Token (_, STRING _) }

%right '->'
-- %right then else
%right prec_if
%right ';'
%nonassoc '==' '<>'
%nonassoc '<' '>' '<=' '>='
%left '&&' '||'
%left '<-'
%left '+' '-' '+.' '-.'
%left '*' '/' '%' '*.' '/.'
%left '.'
%nonassoc NEG

%name parseDecl decl
%name parseExpr exp
%name parseDecls decls
%%

decls : decls_raw { reverse $1 }

decls_raw : decls_raw decl { $2 : $1 }
          |      { [] }

decl : val id ':' Type '=' exp { ValDec (_info $1) (_id . _tag $ $2)
                                 (Just $4)
                                 $6
                               }
     | val id ':=' exp { ValDec (_info $1) (_id . _tag $ $2)
                         Nothing $4
                       }
     | fun id '(' ')' ':' Type '=' exp {
         FunDec (_info $1) (_id . _tag $ $2)
           []
           $6
           $8
       }
     | fun id '(' params ')' ':' Type '=' exp {
         FunDec (_info $1) (_id . _tag $ $2)
           (reverse $4)
           $7
           $9
       }
     | extern id ':' Type '=' str {
         ExDec (_info $1) (_id . _tag $ $2)
           $4
           (_str . _tag $ $6)
       }

params : params ',' param { $3 : $1 }
       | param { [$1] }

param : id ':' Type { (_id . _tag $ $1, Just $3) }
	  | id { (_id . _tag $ $1, Nothing) }

exp: exp '+' exp { BinOp (_info $2) Add $1 $3 }
   | exp '-' exp { BinOp (_info $2) Sub $1 $3 }
   | exp '*' exp { BinOp (_info $2) Mul $1 $3 }
   | exp '/' exp { BinOp (_info $2) Div $1 $3 }
   | simple_exp '.' int { TupleAccess (_info $2) $1 (fromInteger $ _int . _tag $ $3) }
   | exp '+.' exp { BinOp (_info $2) FAdd $1 $3 }
   | exp '-.' exp { BinOp (_info $2) FSub $1 $3 }
   | exp '*.' exp { BinOp (_info $2) FMul $1 $3 }
   | exp '/.' exp { BinOp (_info $2) FDiv $1 $3 }
   | exp '%' exp { BinOp (_info $2) Mod $1 $3 }
   | exp '==' exp { BinOp (_info $2) Eq $1 $3 }
   | exp '<>' exp { BinOp (_info $2) Neq $1 $3 }
   | exp '<' exp { BinOp (_info $2) Lt $1 $3 }
   | exp '>' exp { BinOp (_info $2) Gt $1 $3 }
   | exp '<=' exp { BinOp (_info $2) Le $1 $3 }
   | exp '>=' exp { BinOp (_info $2) Ge $1 $3 }
   | exp '&&' exp { BinOp (_info $2) And $1 $3 }
   | exp '||' exp { BinOp (_info $2) Or $1 $3 }
   | fn '(' params ')' '->' exp { Fn (_info $1) (reverse $3) $6 }
   | let decls in exp end { Let (_info $1) $2 $4 }
   | if exp then exp else exp %prec prec_if { If (_info $1) $2 $4 $6 }
   | exp ';' exp { Seq (_info $2) $1 $3 }
   | '-' int %prec NEG { BinOp (_info $1) Sub
                           (Int (_info $1) 0)
                           (Int (_info $2) (_int . _tag $ $2))
                       }
   | '-' float %prec NEG { BinOp (_info $1) Sub
                             (Float (_info $1) 0)
                             (Float (_info $2) (_float . _tag $ $2))
                         }
   | simple_exp '[' exp ']' '<-' exp { ArrayWrite (_info $5) $1 $3 $6 }
   | simple_exp { $1 }

simple_exp: id { Var (_info $1) (_id . _tag $ $1) }
          | int { Int (_info $1) (_int . _tag $ $1) }
          | float { Float (_info $1) (_float . _tag $ $1) }
          | bool { Bool (_info $1) (_bool . _tag $ $1) }
          | char { Char (_info $1) (_char . _tag $ $1) }
          | str  { String (_info $1) (_str . _tag $ $1) }
          | '{' args '}' { Tuple (_info $1) (reverse $2) }
          | array '(' Type ',' exp ')' { MakeArray (_info $1) $3 $5 }
          | simple_exp '[' exp ']' { ArrayRead (_info $2) $1 $3 }
          | simple_exp '(' ')' {- %prec CALL -} { Call (info $1) $1 [] }
          | simple_exp '(' args ')' {- %prec CALL -} { Call (info $1) $1 (reverse $3) }
          | '{' '}' { Unit (_info $1) }
          | '(' exp ')' { $2 }

args : args ',' exp { $3 : $1 }
     | exp { [$1] }

Type : Int { TyApp IntC [] }
	 | Float { TyApp FloatC [] }
	 | Bool { TyApp BoolC [] }
	 | Char { TyApp CharC [] }
	 | String { TyApp StringC [] }
     | Type '->' Type { TyApp FunC [$3, $1] }
     | '{' '}' { TyApp TupleC [] }
     | '{' Types '}' { TyApp TupleC (reverse $2) }
	 | '(' ')' '->' Type { TyApp FunC [$4] }
     | '(' Types ')' '->' Type { TyApp FunC ($5 : reverse $2) }
     | '[' Type ']' { TyApp ArrayC [$2] }

Types : Types ',' Type { $3 : $1 }
      | Type { [$1] }
{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ "Parse error at EOF: " <> show xs <> " are expected."
parseError (t:_, xs) = error $ "Parse error: " <> show t <> " is got, but " <> show xs <> " are expected."
}

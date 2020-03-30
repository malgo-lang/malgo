-- -*- mode: fundamental -*-
{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE NoStrict #-}
module Language.Malgo.Parser where

import Prelude hiding (EQ, LT, GT)
import Language.Malgo.Token
import Language.Malgo.TypeRep.SType
import Language.Malgo.IR.Syntax
import Language.Malgo.Pretty
import Text.PrettyPrint.HughesPJ (text, hang)
import Data.String
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.List (intercalate)
import Text.Parsec.Pos (SourcePos)
}

%name parse
%tokentype { Token }
%error { parseError }
%errorhandlertype explist

%token
'let'    { Token (_, LET) }
'in'     { Token (_, IN) }
'val'    { Token (_, VAL) }
'fun'    { Token (_, FUN) }
'type'   { Token (_, TYPE) }
'extern' { Token (_, EXTERN) }
'and'    { Token (_, AND) }
'fn'     { Token (_, FN) }
'if'     { Token (_, IF) }
'then'   { Token (_, THEN) }
'else'   { Token (_, ELSE) }
'array'  { Token (_, ARRAY) }
'match'  { Token (_, MATCH) }
'with'   { Token (_, WITH) }
'Int'    { Token (_, TY_INT) }
'Float'  { Token (_, TY_FLOAT) }
'Bool'   { Token (_, TY_BOOL) }
'Char'   { Token (_, TY_CHAR) }
'String' { Token (_, TY_STRING) }
'('   { Token (_, LPAREN) }
')'   { Token (_, RPAREN) }
'['   { Token (_, LBRACK) }
']'   { Token (_, RBRACK) }
'{'   { Token (_, LBRACE) }
'}'   { Token (_, RBRACE) }
':='  { Token (_, COLON_EQUAL) }
':'   { Token (_, COLON) }
';'   { Token (_, SEMICOLON) }
','   { Token (_, COMMA) }
'=='  { Token (_, EQUAL_EQUAL) }
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
'&&'  { Token (_, AND_AND) }
'||'  { Token (_, OR_OR) }
'->'  { Token (_, ARROW) }
'<-'  { Token (_, LARROW) }
'|'   { Token (_, BAR) }
'=>'  { Token (_, DARROW) }
id    { Token (_, ID _) }
lid   { Token (_, LID _) }
float { Token (_, FLOAT _) }
int   { Token (_, INT _) }
char  { Token (_, CHAR _) }
bool  { Token (_, BOOL _) }
str   { Token (_, STRING _) }

%nonassoc 'in'
%right '->'
%right prec_match
%right prec_if
%right '|'
%nonassoc '=>'
%right ';'
%left '&&' '||'
%nonassoc '==' '<>'
%nonassoc '<' '>' '<=' '>='
%left '<-'
%left '+' '-' '+.' '-.'
%left '*' '/' '%' '*.' '/.'
%left '.'
%nonassoc NEG

%name parseDecl decl
%name parseExpr exp
%name parseDecls decls
%%

decls : decls decl { $2 : $1 }
      | { [] }

val_decl : 'val' id ':' type '=' exp { ValDec (_sourcePos $1) (_id $ _tag $ $2) (Just $4) $6 }
         | 'val' id '=' exp { ValDec (_sourcePos $1) (_id $ _tag $ $2) Nothing $4 }

fun_decls : fun_decls_raw { FunDec (reverse $1) }

fun_decls_raw : fun_decls_raw 'and' fun_decl { $3 : $1 }
              | 'fun' fun_decl { [$2] }

fun_decl : id '(' ')' ':' type '=' exp { (_sourcePos $1, _id $ _tag $1, [], Just $5, $7) }
         | id '(' ')' '=' exp { (_sourcePos $1, _id $ _tag $1, [], Nothing, $5) }
         | id '(' params ')' ':' type '=' exp { (_sourcePos $1, _id $ _tag $1, reverse $3, Just $6, $8) }
         | id '(' params ')' '=' exp { (_sourcePos $1, _id $ _tag $1, reverse $3, Nothing, $6) }

ext_decl : 'extern' id ':' type '=' str { ExDec (_sourcePos $1) (_id . _tag $ $2) $4 (_str $ _tag $ $6) }

decl : val_decl { $1 }
     | fun_decls { $1 }
     | ext_decl { $1 }

params : params ',' param { $3 : $1 }
       | param { [$1] }

param : id ':' type { (_id . _tag $ $1, Just $3) }
       | id { (_id . _tag $ $1, Nothing) }

exp: exp '+' exp { BinOp (_sourcePos $2) Add $1 $3 }
   | exp '-' exp { BinOp (_sourcePos $2) Sub $1 $3 }
   | exp '*' exp { BinOp (_sourcePos $2) Mul $1 $3 }
   | exp '/' exp { BinOp (_sourcePos $2) Div $1 $3 }
   | exp '+.' exp { BinOp (_sourcePos $2) FAdd $1 $3 }
   | exp '-.' exp { BinOp (_sourcePos $2) FSub $1 $3 }
   | exp '*.' exp { BinOp (_sourcePos $2) FMul $1 $3 }
   | exp '/.' exp { BinOp (_sourcePos $2) FDiv $1 $3 }
   | exp '%' exp { BinOp (_sourcePos $2) Mod $1 $3 }
   | exp '==' exp { BinOp (_sourcePos $2) Eq $1 $3 }
   | exp '<>' exp { BinOp (_sourcePos $2) Neq $1 $3 }
   | exp '<' exp { BinOp (_sourcePos $2) Lt $1 $3 }
   | exp '>' exp { BinOp (_sourcePos $2) Gt $1 $3 }
   | exp '<=' exp { BinOp (_sourcePos $2) Le $1 $3 }
   | exp '>=' exp { BinOp (_sourcePos $2) Ge $1 $3 }
   | exp '&&' exp { BinOp (_sourcePos $2) And $1 $3 }
   | exp '||' exp { BinOp (_sourcePos $2) Or $1 $3 }
   | 'fn' '(' params ')' '=>' exp { Fn (_sourcePos $1) (reverse $3) $6 }
   | 'let' decls 'in' exp { foldl (\e d -> Let (_sourcePos $1) d e) $4 $2 }
   | 'if' exp 'then' exp 'else' exp %prec prec_if { If (_sourcePos $1) $2 $4 $6 }
   | 'match' exp 'with' '|' clauses { Match (_sourcePos $1) $2 (fromList $ reverse $5) }
   | exp ';' exp { Seq (_sourcePos $2) $1 $3 }
   | '-' int %prec NEG { BinOp (_sourcePos $1) Sub
                           (Int (_sourcePos $1) 0)
                           (Int (_sourcePos $2) (_int . _tag $ $2))
                       }
   | '-' float %prec NEG { BinOp (_sourcePos $1) Sub
                             (Float (_sourcePos $1) 0)
                             (Float (_sourcePos $2) (_float . _tag $ $2))
                         }
   | simple_exp '.' '(' exp ')' '<-' exp { ArrayWrite (_sourcePos $6) $1 $4 $7 }
   | simple_exp { $1 }

clauses : clauses '|' pat '=>' exp { ($3, $5) : $1 }
        | pat '=>' exp { [($1, $3)] }

pat : id { VarP (_id . _tag $ $1) }
    | '{' tuple_pat '}' { TupleP $ reverse $2 }

tuple_pat : tuple_pat ',' pat { $3 : $1 }
          | pat { [$1] }

simple_exp: id { Var (_sourcePos $1) (_id . _tag $ $1) }
          | int { Int (_sourcePos $1) (_int . _tag $ $1) }
          | float { Float (_sourcePos $1) (_float . _tag $ $1) }
          | bool { Bool (_sourcePos $1) (_bool . _tag $ $1) }
          | char { Char (_sourcePos $1) (_char . _tag $ $1) }
          | str  { String (_sourcePos $1) (_str . _tag $ $1) }
          | '{' args '}' { Tuple (_sourcePos $1) (reverse $2) }
          | '[' args ']' { Array (_sourcePos $1) (fromList $ reverse $2) }
          | 'array' '(' exp ',' exp ')' { MakeArray (_sourcePos $1) $3 $5 }
          | simple_exp '.' '(' exp ')' { ArrayRead (_sourcePos $2) $1 $4 }
          | simple_exp '(' ')' {- %prec CALL -} { Call (position $1) $1 [] }
          | simple_exp '(' args ')' {- %prec CALL -} { Call (position $1) $1 (reverse $3) }
          | '{' '}' { Tuple (_sourcePos $1) [] }
          | '(' exp ')' { $2 }

args : args ',' exp { $3 : $1 }
     | exp { [$1] }

type : 'Int' { TyInt }
     | 'Float' { TyFloat }
     | 'Bool' { TyBool }
     | 'Char' { TyChar }
     | 'String' { TyString }
     | id { TyVar $ _id $ _tag $1 }
     | type '->' type { TyFun [$1] $3 }
     | '{' '}' { TyTuple [] }
     | '{' types '}' { TyTuple (reverse $2) }
     | '(' ')' '->' type { TyFun [] $4 }
     | '(' types ')' '->' type { TyFun (reverse $2) $5 }
     | '[' type ']' { TyArray $2 }

types : types ',' type { $3 : $1 }
      | type { [$1] }
{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ show $ "Parse error at EOF: " <> pPrint xs <> " are expected."
parseError (t:_, xs) = error $ show $ hang "Parse error: " 0 $ pPrint t <> " is got, but " <> text (intercalate "," xs) <> " are expected."
}

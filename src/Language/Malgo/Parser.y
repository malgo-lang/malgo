-- -*- mode: fundamental -*-
{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE NoStrict #-}
module Language.Malgo.Parser where

import Prelude hiding (EQ, LT, GT)
import Language.Malgo.Token
import Language.Malgo.TypeRep.Type
import Language.Malgo.IR.Syntax
import Language.Malgo.FrontEnd.Info
import Language.Malgo.Pretty
import Language.Malgo.Prelude (toText)
import Text.PrettyPrint.HughesPJ (text, hang)
import Data.String
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.List (intercalate)
}

%name parse
%tokentype { Token }
%error { parseError }
%errorhandlertype explist

%token
let   { Token (_, LET) }
in    { Token (_, IN) }
val   { Token (_, VAL) }
fun   { Token (_, FUN) }
type  { Token (_, TYPE) }
extern { Token (_, EXTERN) }
fn { Token (_, FN) }
if    { Token (_, IF) }
then  { Token (_, THEN) }
else  { Token (_, ELSE) }
array { Token (_, ARRAY) }
match { Token (_, MATCH) }
with  { Token (_, WITH) }
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
':='  { Token (_, COLON_EQUAL) }
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
'|'   { Token (_, BAR) }
'=>'  { Token (_, DARROW) }
id    { Token (_, ID _) }
float { Token (_, FLOAT _) }
int   { Token (_, INT _) }
char  { Token (_, CHAR _) }
bool  { Token (_, BOOL _) }
str   { Token (_, STRING _) }

%nonassoc in
%right '->'
%right prec_match
%right prec_if
%right '|'
%nonassoc '=>'
%right ';'
%left '&&' '||'
%nonassoc '==' '<>'
%nonassoc '<' '>' '<=' '>='
%left ':='
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
          | { [] }

val_decl : val id ':' Type '=' exp { V (_info $1) (_id $ _tag $ $2) (Just $4) $6 }
         | val id '=' exp { V (_info $1) (_id $ _tag $ $2) Nothing $4 }

fun_decl : fun id '(' ')' ':' Type '=' exp { F (_info $1) (_id . _tag $ $2) [] (Just $6) $8 }
         | fun id '(' ')' '=' exp { F (_info $1) (_id . _tag $ $2) [] Nothing $6 }
         | fun id '(' params ')' ':' Type '=' exp { F (_info $1) (_id . _tag $ $2) (reverse $4) (Just $7) $9 }
         | fun id '(' params ')' '=' exp { F (_info $1) (_id . _tag $ $2) (reverse $4) Nothing $7 }

ext_decl : extern id ':' Type '=' str { E (_info $1) (_id . _tag $ $2) $4 (_str $ _tag $ $6) }

decl : val_decl { $1 }
     | fun_decl { $1 }
     | ext_decl { $1 }

params : params ',' param { $3 : $1 }
       | param { [$1] }

param : id ':' Type { (_id . _tag $ $1, Just $3) }
       | id { (_id . _tag $ $1, Nothing) }

exp: exp '+' exp { BinOp (_info $2) Add $1 $3 }
   | exp '-' exp { BinOp (_info $2) Sub $1 $3 }
   | exp '*' exp { BinOp (_info $2) Mul $1 $3 }
   | exp '/' exp { BinOp (_info $2) Div $1 $3 }
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
   | let decls in exp { toLet (_info $1) $2 $4 }
   | if exp then exp else exp %prec prec_if { If (_info $1) $2 $4 $6 }
   | match exp with '|' clauses { Match (_info $1) $2 (fromList $ reverse $5) }
   | exp ';' exp { Seq (_info $2) $1 $3 }
   | '-' int %prec NEG { BinOp (_info $1) Sub
                           (Int (_info $1) 0)
                           (Int (_info $2) (_int . _tag $ $2))
                       }
   | '-' float %prec NEG { BinOp (_info $1) Sub
                             (Float (_info $1) 0)
                             (Float (_info $2) (_float . _tag $ $2))
                         }
   | simple_exp '[' exp ']' ':=' exp { ArrayWrite (_info $5) $1 $3 $6 }
   | simple_exp { $1 }

clauses : clauses '|' pat '->' exp { ($3, $5) : $1 }
        | pat '->' exp { [($1, $3)] }

pat : id { VarP (_id . _tag $ $1) }
    | '{' tuple_pat '}' { TupleP $ reverse $2 }

tuple_pat : tuple_pat ',' pat { $3 : $1 }
          | pat { [$1] }

simple_exp: id { Var (_info $1) (_id . _tag $ $1) }
          | int { Int (_info $1) (_int . _tag $ $1) }
          | float { Float (_info $1) (_float . _tag $ $1) }
          | bool { Bool (_info $1) (_bool . _tag $ $1) }
          | char { Char (_info $1) (_char . _tag $ $1) }
          | str  { String (_info $1) (_str . _tag $ $1) }
          | '{' args '}' { Tuple (_info $1) (reverse $2) }
          | '[' args ']' { Array (_info $1) (fromList $ reverse $2) }
          | array '(' exp ',' exp ')' { MakeArray (_info $1) $3 $5 }
          | simple_exp '[' exp ']' { ArrayRead (_info $2) $1 $3 }
          | simple_exp '(' ')' {- %prec CALL -} { Call (info $1) $1 [] }
          | simple_exp '(' args ')' {- %prec CALL -} { Call (info $1) $1 (reverse $3) }
          | '{' '}' { Tuple (_info $1) [] }
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
parseError ([], xs) = error $ show $ "Parse error at EOF: " <> pPrint xs <> " are expected."
parseError (t:_, xs) = error $ show $ hang "Parse error: " 0 $ pPrint t <> " is got, but " <> text (intercalate "," xs) <> " are expected."

data D = V Info String (Maybe Type) (Expr String)
       | F Info String [(String, Maybe Type)] (Maybe Type) (Expr String)
       | E Info String Type String

toLet :: Info -> [D] -> Expr String -> Expr String
toLet _ [] = id
toLet li (V i n t e : xs) = Let li (ValDec i n t e) . toLet li xs
toLet li xs@(F{} : _) =
  let (fundecs, rest) = span isF xs
  in Let li (FunDec $ map toFunDec fundecs) . toLet li rest
toLet li (E i n t o : xs) = Let li (ExDec i n t o). toLet li xs

isF :: D -> Bool
isF F{} = True
isF _ = False

toFunDec :: D -> _
toFunDec (F i f ps r e) = (i, f, ps, r, e)
}

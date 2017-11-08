{
module Language.Malgo.Parser where

import Prelude hiding (EQ, LT, GT)
import Language.Malgo.Lexer
import Language.Malgo.Syntax
import Language.Malgo.Utils
}

%name parse
%tokentype { Token }
%error { parseError }

%token
let   { Token (_, LET) }
in    { Token (_, IN) }
end   { Token (_, END) }
val   { Token (_, VAL) }
fun   { Token (_, FUN) }
if    { Token (_, IF) }
then  { Token (_, THEN) }
else  { Token (_, ELSE) }
'('   { Token (_, LPAREN) }
')'   { Token (_, RPAREN) }
':'   { Token (_, COLON) }
';'   { Token (_, SEMICOLON) }
'=='  { Token (_, EQ) }
'='   { Token (_, EQUAL) }
'<>'  { Token (_, NEQ) }
'<'   { Token (_, LT) }
'>'   { Token (_, GT) }
'<='  { Token (_, LE) }
'>='  { Token (_, GE) }
'+'   { Token (_, PLUS) }
'-'   { Token (_, MINUS) }
'*'   { Token (_, ASTERISK) }
'/'   { Token (_, SLASH) }
'%'   { Token (_, PERCENT) }
'&&'  { Token (_, AND) }
'||'  { Token (_, OR) }
id    { Token (_, ID _) }
float { Token (_, FLOAT _) }
int   { Token (_, INT _) }
char  { Token (_, CHAR _) }
bool  { Token (_, BOOL _) }
str   { Token (_, STRING _) }

%right then else
%nonassoc '==' '<>'
%nonassoc '<' '>' '<=' '>='
%left '&&' '||'
%left '+' '-'
%left '*' '/' '%'
%nonassoc NEG
%%

exp: exp '+' exp { BinOp (_info $2) Add $1 $3 }
   | exp '-' exp { BinOp (_info $2) Sub $1 $3 }
   | exp '*' exp { BinOp (_info $2) Mul $1 $3 }
   | exp '/' exp { BinOp (_info $2) Div $1 $3 }
   | exp '%' exp { BinOp (_info $2) Mod $1 $3 }
   | exp '==' exp { BinOp (_info $2) Eq $1 $3 }
   | exp '<>' exp { BinOp (_info $2) Neq $1 $3 }
   | exp '<' exp { BinOp (_info $2) Lt $1 $3 }
   | exp '>' exp { BinOp (_info $2) Gt $1 $3 }
   | exp '<=' exp { BinOp (_info $2) Le $1 $3 }
   | exp '>=' exp { BinOp (_info $2) Ge $1 $3 }
   | exp '&&' exp { BinOp (_info $2) And $1 $3 }
   | exp '||' exp { BinOp (_info $2) Or $1 $3 }
   | simple_exp { $1 }

simple_exp: id { Var (_info $1) (_id . _tag $ $1) }
          | int { Int (_info $1) (_int . _tag $ $1) }
          | '-' int %prec NEG { BinOp (_info $1) Sub
                                  (Int (_info $1) 0)
                                  (Int (_info $2) (_int . _tag $ $2))
                              }
          | float { Float (_info $1) (_float . _tag $ $1) }
          | '-' float %prec NEG { BinOp (_info $1) Sub
                                    (Float (_info $1) 0)
                                    (Float (_info $2) (_float . _tag $ $2))
                                }
          | bool { Bool (_info $1) (_bool . _tag $ $1) }
          | char { Char (_info $1) (_char . _tag $ $1) }
          | str  { String (_info $1) (_str . _tag $ $1) }
          | '(' ')' { Unit (_info $1) }
          | '(' exp ')' { $2 }

{
parseError :: [Token] -> a
parseError [] = error "Parse error at EOF"
parseError (t:ts) = error $ "Parse error:" ++ show t
}

# Malus

## 文法
```
import "std"; // 基本の文法はGo風

extern putchar(i32) i32;

def main() i32 {
    putchar(65);
    var hello [i32] = [72, 101, 108, 108, 111];
    for (var i i32 = 0; i < length(hello); i = i + 1) {
       putchar(i);
    }
    return 0; // mainの返り値はステータスコード
}
```

{}は0回以上の繰り返し、[]はオプション

```
translation_unit : { toplevel };

toplevel : import
         | definition
         | external
         ;

import : 'import' string_lit;

definition : 'def' prototype expression;

external : 'extern' prototype;

prototype : id '(' [ parameter { ',' parameter } ] ')' id;

parameter : id id;

assing_expr : var_declaration '=' expression;
var_declaration : 'var' id id
                | id
                ;

expression : primary [ binoprhs ]
           | assign_expr;
           | binoprhs // 単項演算子

primary : id
        | literal
        | paren
        | funcall
        ;

binoprhs : (op primary)+;
op : '+' | '-' | '*' | '/' | '%';

id : (a-zA-Z)(a-zA-Z0-9)*;

literal : string_lit
        | number_lit;

string_lit : '"' /* なんでも */ '"'
number_lit : (0-9)+['.'(0-9)*]

paren : '(' expression ')'

funcall : id '(' [ expression { ',' expression } ] ')'
```

字句解析は手書きでもツラくないはず。
構文解析は何らかのツールがほしい。
Common LispにはCL-YACCがあるが、有用に使えるかは検討しないといけない。
意味解析は構文解析のあと、ASTをこねくりまわす段階で扱う。
ASTはS式で表現。

## 実装

### AST
```
(import (stringL "std"))
(define_extern (symbol "putchar") ((type i32)) (type i32))
(define_function (symbol "main") () (type i32)
  (block (call (symbol "putchar" ((intL 65))))
         (define_variable (symbol "hello") (type (vector i32)) (vector 72 101 108 108 111))
         ()
         (return 0)))
```

最終的にLLVM IRを吐きたい。
バッファ作ってformat芸キメてるのが良さそう。
まずはインタプリタを作ればいい？
型システムも考えないといけない。

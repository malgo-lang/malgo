# Malgo リファレンスマニュアル

Malgoは、多相型、型推論、カリー化、パターンマッチなどの機能を持つ静的型付き関数プログラミング言語である。

## 字句

### コメント
`--`から行末まではコメントになる。
`{- -}`で囲んだ部分もコメントになる。これは入れ子になってもよい。

### 識別子
識別子は、正規表現`[a-zA-Z_][a-zA-Z0-9_#']*`で表現される文字列、あるいは`+-*/\%=><:;|&!#.`の一文字以上の繰り返しで表現される文字列である。

この文書では、
* `Int32`、`Type`のように大文字から始まる識別子を**upper id**
* `putStrLn`、`main`のように小文字から始まる識別子を**lower id**
* `->`、`+`のように記号から構成される識別子を**operator id**
と呼ぶ。

### 予約語
以下の文字列は予約語であり、識別子には使えない。

`data, exists, forall, foreign, import, infix, infixl, infixr, let, type, module`

### リテラル

リテラルには、末尾に`#`がつく**unboxedリテラル**と、通常の値を表す**boxedリテラル**がある。
それぞれのリテラルの例を列挙する。

* **数値リテラル**
  * 32bit符号付き整数 : unboxed `42#`, boxed `42`
  * 64bit符号付き整数 : unboxed `42L#`, boxed `42L`
  * 単精度浮動小数点数 : unboxed `3.14F#`, boxed `3.14F`
  * 倍精度浮動小数点数 : unboxed `3.14#`, boxed `3.14`
* **文字リテラル** : unboxed `'a'#`, boxed `'a'`
* **文字列リテラル** : unboxed `"hello"#`, boxed `"hello"`

boxedリテラルは、それぞれ対応する関数の呼び出しに変換される。
それぞれの関数の実装はruntime/malgo/Builtin.mlgに存在する。

* `42` -> `int32# 42#`
* `42L` -> `int64# 42L#`
* `3.14F` -> `float# 3.14F#`
* `3.14` -> `double# 3.14#`
* `'a'` -> `char# 'a'#`
* `"hello"` -> `string# "hello"#`

## 宣言

### モジュール

```ebnf
module = "module" upper_id "=" "{" decs "}" ;

decs = { dec ";" } ;

dec = data_dec | type_dec | infix_dec | foreign_dec | import_dec
    | signature_dec | function_dec ;
```

モジュールは一連の型や関数宣言の並びのまとまりであり、名前空間として振る舞う。
例えば、モジュール`List`で宣言された関数`map`と、モジュール`Set`で宣言された関数`map`は別の名前として扱われる。

### 関数宣言

```ebnf
signature_dec = lower_id "::" type ;

function_dec = lower_id "=" "{" { pat } "->" stmts "}"
             | lower_id "=" "{" stmts "}" ;
```

```
fib :: Int32 -> Int32;
fib = { n -> if (n <= 1) { 1 } { fib (n - 1) + fib (n - 2) } };
```

モジュールに定義できる関数は、型`{_}`か型`_ -> _`を持つものに限られる。

### データ型宣言

```ebnf
data_dec = "data" upper_id { lower_id } "=" con_defs ;

con_defs = con_def "|" con_defs
         | con_def ;

con_def = upper_ident { type } ;
```

```
data List a = Nil | Cons a (List a);

data Either a b = Left a | Right b;
```

### 結合性宣言

```ebnf
infix_dec = "infix" decimal "(" operator_id ")"
          | "infixl" decimal "(" operator_id ")"
          | "infixr" decimal "(" operator_id ")"
```

```
infixl 6 (+);
(+) = { x y -> add_Int32 x y };
```

### 外部関数宣言

```ebnf
foreign_dec = "foreign" "import" lower_ident "::" type ;
```

### インポート宣言

```ebnf
import_dec = "import" upper_id ;
```
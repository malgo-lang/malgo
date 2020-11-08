# Griff

Griffは、静的型付けの関数プログラミング言語です。Haskellに影響を受けた構文をもち、正格評価をデフォルトとしつつも軽量な遅延評価構文を備えています。

## Hello, world

Griffで書かれたHello, worldの例です。

Hello.grf
```
package hello; -- package名の宣言

-- 外部関数のインポート
foreign import print_string :: String# -> ();
foreign import newline :: () -> ();

-- 型と値コンストラクタの定義
data String = String# String#;

-- 関数定義
putStrLn :: String -> ();
putStrLn = { (String# str) ->
  print_string str;
  newline ()
};

-- main関数の定義
main = {
  putStrLn (String# "Hello, world"#)
};
```

Hello.grfは以下のようにコンパイル、実行します。
コンパイルにはlibgcが必要です。

```sh
$ griffc Hello.grf -o Hello.o
$ clang -lgc Hello.o runtime/griff/rts.c
$ ./a.out
Hello, world
```

## 関数リテラルとパターンマッチ

Griffの関数リテラルは`{ 引数1 引数2 ... -> 本体 }`のように記述します。
また、引数の部分ではパターンマッチを行う事もできます。

```
-- 中置演算子の定義
infixl 0 (|>);
(|>) :: a -> (a -> b) -> b;
(|>) = {x f -> f x};

-- 連結リストの定義
data List a = Nil | Cons a (List a);

-- リストの総和を求める関数sum
sum =
  -- パターンマッチ
  -- 節は|で区切る
  { Nil -> Nil
  | (Cons (Int# x) xs) ->
      sum xs |> { (Int# s) -> Int# (x +# s) }
  };
```

## 遅延評価

式を`{}`で囲むことで、サンクを作ることができます。
サンクは後置演算子`!`で評価できます。
サンクの型も`{}`で囲んで表現します。

```
data Bool = False | True;

if :: Bool -> {a} -> {a} -> a;
if c t f = c |> { True -> t!
                | False -> f!
                };
```

## 外部関数のインポート

`foreign import 外部関数名 :: 型;`と記述することで、Cなどで書かれた外部関数を利用することができます。
例えば、Hello, worldの例で出てきた`print_string`関数は以下のようなCの関数として定義しています。

```c
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

typedef struct {
  uint8_t tag;
  struct {} payload;
} Unit;

const Unit unit = {0, {}};

const Unit* print_string(char* x) {
  printf("%s", x);
  return &unit;
}
```
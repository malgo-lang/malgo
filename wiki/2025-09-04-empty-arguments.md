# empty-arguments

Created: 2025-09-04

空の引数について考える。

```malgo
def f = { () -> 1 }
f() === 1
```

カリー化との組み合わせがややこしい。

まずは空の引数がない場合を考える。

```malgo
def f = { (x, y) -> x + y }
f(1, 2) === 3
f(1) === { (y) -> 1 + y }
f(1)(2) === 3
f(1)(2)() // error: 3 is not a function

def g = { (x) -> (y) -> x + y }
f === g
```

次に空の引数がある場合を考える。

```malgo
def f = { (x, y) -> x + y }
f(1, 2) === 3
f(1) === { (y) -> 1 + y }
f(1)(2) === 3
f(1)(2)() // error: 3 is not a function

def g = { (x) -> (y) -> x + y }
f === g

def h = { (x) -> (y) -> () -> x + y }
h(1)(2)() === 3
h(1, 2) === 3 or { () -> 1 + 2 } ?
```

`h(1, 2)` は `{ () -> 1 + 2 }` とするのが自然に思えるが、`f(1, 2)` と同じように `3` としたい気もする。

前者は0引数と1引数の２パターンを考慮するだけでいいが、後者は引数充足の判定が複雑になる。

regular syntaxに空の引数を導入するのは、互換性の問題がある。Featureを使って、c style syntaxでのみ空の引数を許可する。

値コンストラクタの扱いを考える。

```malgo
data List(a) = Nil | Cons(a, List(a))

data List2(a) = Nil2() | Cons2(a, List2(a))
```

今は`List`の方のみを許している。`List2`の方を許すなら、値コンストラクタでも空の引数を許す必要がある。

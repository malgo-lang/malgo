# malgo

[![Malgo CI](https://github.com/malgo-lang/malgo/workflows/Malgo%20CI/badge.svg)](https://github.com/malgo-lang/malgo/actions?query=workflow%3A%22Malgo+CI%22)

A statically typed functional programming language.

## Requirement

* [stack](https://docs.haskellstack.org/en/stable/README/)
* [bdwgc](http://www.hboehm.info/gc/)
* [clang](https://clang.llvm.org/)
* [llvm](https://llvm.org/)

## Installation

### Installing LLVM
#### Homebrew

```sh
$ brew install llvm-hs/llvm/llvm-12
```

#### Debian/Ubuntu

```
$ apt-get install llvm-9-dev
```

### Installing Malgo

```sh
$ git clone https://github.com/malgo-lang/malgo
$ cd malgo
$ stack install
$ ./scripts/install_malgo_internal.sh
```

## Usage

```sh
$ ./scripts/compile.sh examples/malgo/Hello.mlg
$ ./Hello
Hello, world
```

## Examples

### Hello, world

```
module Hello = {
  module {..} = import Builtin;
  module {..} = import Prelude;

  main = {
    putStrLn "Hello, world!"
  };
}
```

### Fibonacci number

```
module Fib = {
  module {..} = import Builtin;
  module {..} = import Prelude;

  infix 4 (<=);
  (<=) = { x y -> leInt32 x y };

  infixl 6 (+);
  (+) = { x y -> addInt32 x y };

  infixl 6 (-);
  (-) = { x y -> subInt32 x y };

  fib = { n ->
    if (n <= 1)
      { 1 }
      { fib (n - 1) + fib (n - 2) }
  };

  main = {
    fib 5 |> toStringInt32 |> putStrLn
  };
}
```

### List operations

```
module List = {
  module {..} = import Builtin;
  module {..} = import Prelude;

  infix 4 (<=);
  (<=) : Int32 -> Int32 -> Bool;
  (<=) = {x y -> leInt32 x y};

  infixl 6 (+);
  (+) : Int32 -> Int32 -> Int32;
  (+) = {x y -> addInt32 x y};

  infixl 6 (-);
  (-) : Int32 -> Int32 -> Int32;
  (-) = {x y -> subInt32 x y};

  map : (a -> b) -> List a -> List b;
  map = { _ Nil -> Nil
        | f (Cons x xs) -> Cons (f x) (map f xs)
        };

  sum : List Int32 -> Int32;
  sum = { Nil -> 0
        | Cons x xs -> x + sum xs
        };

  -- [0 .. n]
  below : Int32 -> List Int32;
  below = { n ->
    if (n <= 0)
       { [0] }
       { Cons n (below (n - 1)) }
  };

  main = {
      sum (map (addInt32 1) (below 10))
        |> toStringInt32
        |> putStrLn
  };
}
```

### Lisp interpreter

https://github.com/malgo-lang/minilisp

## TODO
[Malgo タスクリスト \- 星にゃーんのScrapbox](https://scrapbox.io/takoeight0821/Malgo_%E3%82%BF%E3%82%B9%E3%82%AF%E3%83%AA%E3%82%B9%E3%83%88)

## Compilation issue on macOS

In my case, `stack build` says:

```
llvm-hs> error: dyld[89679]: Library not loaded: @rpath/libc++abi.1.dylib
llvm-hs>   Referenced from: /usr/local/Cellar/llvm-12/12_2/lib/llvm-12/lib/libc++.1.0.dylib
llvm-hs>   Reason: tried: '/usr/local/lib/libc++abi.1.dylib' (no such file), '/usr/lib/libc++abi.1.dylib' (no such file)
```

I don't know exactly what caused this. However, I was able to solve it with `ln -s /usr/local/lib/libc++abi.1.dylib /usr/local/Cellar/llvm-12/12/12_2/lib/llvm-12/lib/lib/libc++.1.0.dylib`.

## How to Test

```sh
# full test (parallel)
cabal test --test-show-details=streaming
# full test (serial)
cabal test --test-show-details=streaming --test-options='-j1'
# usual case (all optimization enabled) only
cabal test --test-show-details=streaming --test-options='--match "usual"'
# no all optimization case only
cabal test --test-show-details=streaming --test-options='--match "nono"'
# no lambda-lift optimization case only
cabal test --test-show-details=streaming --test-options='--match "nolift"'
# no other optimization case only
cabal test --test-show-details=streaming --test-options='--match "noopt"'

cabal exec malgo 

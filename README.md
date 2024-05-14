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
-- Modules can be imported by file path
module {..} = import "./Builtin.mlg"
module {..} = import "./Prelude.mlg"

def main : () -> ()
def main = {
  putStrLn "Hello, world!"
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
```
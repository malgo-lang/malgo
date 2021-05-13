# malgo

[![Malgo CI](https://github.com/takoeight0821/malgo/workflows/Malgo%20CI/badge.svg)](https://github.com/takoeight0821/malgo/actions?query=workflow%3A%22Malgo+CI%22)

## Requirement

* [stack](https://docs.haskellstack.org/en/stable/README/)
* [bdwgc](http://www.hboehm.info/gc/)
* [clang](https://clang.llvm.org/)
* [llvm](https://llvm.org/)

## Installation

### Installing LLVM
#### Homebrew

```sh
$ brew install llvm-hs/llvm/llvm-9
```

#### Debian/Ubuntu

```
$ apt-get install llvm-9-dev
```

### Installing Malgo

```sh
$ git clone https://github.com/takoeight0821/malgo
$ cd malgo
$ stack install
$ ./install_malgo_internal.sh
```

## Usage

```sh
$ malgo to-ll examples/malgo/Hello.mlg -o Hello.ll
$ clang Hello.ll -lgc .malgo-work/build/Builtin.ll .malgo-work/build/Prelude.ll runtime/malgo/rts.c
$ ./a.out
Hello, world
```

## Koriel

Some utilities for Kagami and the implementation of the Core internal representation.

## Malgo

A statically typed functional programming language.

# TODO

* Rich build tool like go, cargo, cabal

* Malgo
  + Import Builtin.grf implicity
  + Mutable (unboxed | boxed) polymorphic array (in Koriel, Array)
  + More rich standard library
  + ML like module system
  + Overloaded Literals
* Koriel
  + Rename Core -> Koriel
  + Syntax and Parser
    - Ref #9
  + Support overloaded function

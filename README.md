# KaGaMi

[![Kagami CI](https://github.com/takoeight0821/kagami/workflows/Kagami%20CI/badge.svg)](https://github.com/takoeight0821/kagami/actions?query=workflow%3A%22Kagami+CI%22)

Kagami is a collection of toy programming language implementaions and toolchain.

## Requirement

* [stack](https://docs.haskellstack.org/en/stable/README/)
* [bdwgc](http://www.hboehm.info/gc/)
* [clang](https://clang.llvm.org/)

## Installation

```sh
$ git clone https://github.com/takoeight0821/kagami
$ cd kagami
$ stack install
```

## Usage

```sh
$ malgoc examples/malgo/Hello.grf -o Hello.ll
$ clang Hello.ll runtime/malgo/rts.c -lgc
$ ./a.out
Hello, world
```

## Koriel

Some utilities for Kagami and the implementation of the Core internal representation.

## Malgo

A functional programming language.
Its syntax is based on Haskell.

# Future works

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

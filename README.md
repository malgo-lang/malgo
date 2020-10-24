# KaGaMi

[![Build Status](https://travis-ci.org/takoeight0821/kagami.svg?branch=master)](https://travis-ci.org/takoeight0821/kagami)

Kagami is a collection of toy programming language implementaions and toolchain.

## Koriel

Some utilities for Kagami and the implementation of the Core internal representation.

## Malgo

A simple functional programming language inspired by MinCaml and Tiger.

## Griff

A functional programming language.
Its syntax is based on Haskell.

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
$ malgo examples/hello.mlg -o hello.ll
$ clang hello.ll runtime/malgo/rts.c -lgc
$ ./a.out
Hello, world

$ griff examples/griff/Hello.grf -o Hello.ll
$ clang Hello.ll runtime/griff/rts.c -lgc
$ ./a.out
Hello, world
```

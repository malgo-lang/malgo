# KaGaMi

[![Build Status](https://travis-ci.org/takoeight0821/malgo.svg?branch=master)](https://travis-ci.org/takoeight0821/malgo)

TODO: A short description of KaGaMi project.

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
$ clang hello.ll examples/corelib.c -lgc
$ ./a.out
Hello, world

$ griff examples/griff/Hello.grf -o Hello.ll
$ clang Hello.ll examples/griff/lib.c -lgc
$ ./a.out
Hello, world
```

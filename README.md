# Malgo

[![Build Status](https://travis-ci.org/takoeight0821/malgo.svg?branch=master)](https://travis-ci.org/takoeight0821/malgo)

A toy pragramming language.

## Requirement

* [stack](https://docs.haskellstack.org/en/stable/README/)
* [bdwgc](http://www.hboehm.info/gc/)
* [clang](https://clang.llvm.org/)

## Installation

```sh
$ git clone https://github.com/takoeight0821/malgo
$ cd malgo
$ stack install
```

## Usage

```sh
$ malgo examples/fib.mlg > fib.ll
$ clang fib.ll examples/lib.c -lgc
$ ./a.out
1346269
832040
...
5
3
2
1
1
```

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
$ malgo examples/hello.mlg -o hello.ll
$ clang hello.ll examples/corelib.c -lgc
$ ./a.out
Hello, world
```

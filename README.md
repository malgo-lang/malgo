# malgo

[![Malgo CI](https://github.com/malgo-lang/malgo/workflows/Malgo%20CI/badge.svg)](https://github.com/malgo-lang/malgo/actions?query=workflow%3A%22Malgo+CI%22)

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

## Koriel

The implementation of the internal representation.

## Malgo

A statically typed functional programming language.

# TODO

* Rich build tool like go, cargo, cabal

* Malgo
  + Import Builtin.mlg implicity
  + Mutable (unboxed | boxed) polymorphic array (in Koriel, Array)
  + Row polymorphism
  + More rich standard library
  + ML like module system
  + Overloaded Literals
* Koriel
  + Rename Core -> Koriel
  + Syntax and Parser
    - Ref #9
  + Support overloaded function

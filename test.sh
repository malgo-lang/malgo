#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
mkdir ./tmp
cabal build
ls ./examples | grep mlg | xargs -I{} sh -c 'echo {} && cabal exec malgo -- ./examples/{} -o ./tmp/{}.ll && clang -lgc ./examples/lib.c ./tmp/{}.ll && rm ./tmp/{}.ll && ./a.out'
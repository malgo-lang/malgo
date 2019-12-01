#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
mkdir ./tmp
ls ./examples | grep mlg | xargs -I{} sh -c 'echo {}; cabal exec malgo -- ./examples/{} -o ./tmp/{}.ll; clang -lgc ./examples/lib.c ./tmp/{}.ll; ./a.out' && rm ./tmp/*.ll

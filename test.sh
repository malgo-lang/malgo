#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
mkdir ./tmp
stack build
ls ./examples | grep mlg | xargs -P8 -I{} sh -c 'echo {} && stack exec malgo -- ./examples/{} -o ./tmp/{}.ll && clang -lgc ./examples/lib.c ./tmp/{}.ll && ./a.out'
rm ./tmp/*.ll
rm ./a.out
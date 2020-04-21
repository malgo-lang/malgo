#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
mkdir ./tmp
stack build
ls ./examples | grep mlg | xargs -I{} sh -c 'echo {} && stack exec malgo -- ./examples/{} -o ./tmp/{}.ll && clang -lgc ./examples/lib.c ./tmp/{}.ll -o ./tmp/{}.out && ./tmp/{}.out || exit 255' && rm ./tmp/*.ll && rm ./tmp/*.out

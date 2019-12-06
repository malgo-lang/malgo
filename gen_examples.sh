#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
mkdir ./examples/gen
rm -f ./examples/gen/*
ls ./examples | grep mlg | xargs -I{} sh -c 'echo {} && cabal exec malgo -- ./examples/{} -o ./examples/gen/{}.ll && clang -lgc ./examples/lib.c ./examples/gen/{}.ll && ./a.out'

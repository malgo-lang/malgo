#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
mkdir ./tmp
ls ./examples | grep mlg | xargs -I{} sh -c 'echo {}; malgo ./examples/{} -o ./tmp/{}.ll; clang -lgc ./examples/lib.c ./tmp/{}.ll'
rm ./tmp/*.ll

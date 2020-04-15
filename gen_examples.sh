#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
mkdir ./examples/gen
rm -f ./examples/gen/*
stack build
ls ./examples | grep mlg | xargs -I{} sh -c 'echo {} && stack exec malgo -- ./examples/{} -o ./examples/gen/{}.ll && clang -emit-llvm -S -O2 ./examples/gen/{}.ll -o ./examples/gen/{}_opt.ll'

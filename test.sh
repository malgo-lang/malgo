#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
ls ./examples | grep mlg | xargs -I{} sh -c 'echo {}; malgo ./examples/{}; clang -lgc ./examples/lib.c out.ll'

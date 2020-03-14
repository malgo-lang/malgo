#!/usr/bin/env bash

# examplesがコンパイルできるかチェック
mkdir ./tmp

for file in `ls ./examples | grep mlg`; do
  echo $file
  if stack exec malgo -- "./examples/${file}" -o "./tmp/${file}.ll" && clang -lgc ./examples/lib.c "./tmp/${file}.ll" && rm "./tmp/${file}.ll" && ./a.out; then
    echo "PASS"
  else
    exit 1
  fi
done
#!/usr/bin/env bash

# testcasesがコンパイルできるかチェック
mkdir ./tmp
stack build

for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file ==="
  stack exec malgo -- ./testcases/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/lib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || `echo "FAIL!!" && exit 255`
  echo 'SUCCESS!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file ==="
  stack exec malgo -- ./testcases/bug/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/lib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || echo "FAIL!!"
  echo 'SUCCESS!!'
done

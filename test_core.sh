#!/usr/bin/env bash

# testcasesがコンパイルできるかチェック
mkdir ./tmp

for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file ==="
  stack exec malgo -- --no-opt --core-mode ./testcases/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/corelib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file ==="
  stack exec malgo -- --core-mode ./testcases/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/corelib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file ==="
  stack exec malgo -- --no-opt --core-mode ./testcases/bug/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/corelib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || echo 'FAIL!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file ==="
  stack exec malgo -- --core-mode ./testcases/bug/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/corelib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || echo 'FAIL!!'
done

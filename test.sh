#!/usr/bin/env bash

# testcasesがコンパイルできるかチェック
rm ./tmp/*

for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file no opt ==="
  cabal exec malgo -- --no-opt --no-lambdalift ./testcases/$file -o ./tmp/$file.ll && \
  cat ./tmp/$file.ll && \
  clang -lgc ./examples/corelib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file ==="
  cabal exec malgo -- ./testcases/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/corelib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file no opt ==="
  cabal exec malgo -- --no-opt --no-lambdalift ./testcases/bug/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/corelib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || echo 'FAIL!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file ==="
  cabal exec malgo -- ./testcases/bug/$file -o ./tmp/$file.ll && \
  clang -lgc ./examples/corelib.c ./tmp/$file.ll -o ./tmp/$file.out && \
  ./tmp/$file.out && \
  rm ./tmp/$file.ll && \
  rm ./tmp/$file.out || echo 'FAIL!!'
done

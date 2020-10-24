#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir $TESTDIR

# testcasesがコンパイルできるかチェック
for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file no opt ==="
  cabal exec malgo -- --no-opt --no-lambdalift ./testcases/$file -o $TESTDIR/$file.ll && \
  cat $TESTDIR/$file.ll && \
  clang -lgc ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file ==="
  cabal exec malgo -- ./testcases/$file -o $TESTDIR/$file.ll && \
  clang -lgc ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file no opt ==="
  cabal exec malgo -- --no-opt --no-lambdalift ./testcases/bug/$file -o $TESTDIR/$file.ll && \
  clang -lgc ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || echo 'FAIL!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file ==="
  cabal exec malgo -- ./testcases/bug/$file -o $TESTDIR/$file.ll && \
  clang -lgc ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || echo 'FAIL!!'
done

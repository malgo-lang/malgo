#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir $TESTDIR

# testcasesがコンパイルできるかチェック
for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file no opt ==="
  stack exec malgoc -- --no-opt --no-lambdalift ./testcases/$file -o $TESTDIR/$file.ll && \
  cat $TESTDIR/$file.ll && \
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file ==="
  stack exec malgoc -- ./testcases/$file -o $TESTDIR/$file.ll && \
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases | grep mlg`; do
  echo -e "\n=== $file via binding ==="
  stack exec malgoc -- --via-binding ./testcases/$file -o $TESTDIR/$file.ll && \
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || exit 255
  echo 'SUCCESS!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file no opt ==="
  stack exec malgoc -- --no-opt --no-lambdalift ./testcases/bug/$file -o $TESTDIR/$file.ll && \
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || echo 'FAIL!!'
done

for file in `ls ./testcases/bug | grep mlg`; do
  echo -e "\n=== $file ==="
  stack exec malgoc -- ./testcases/bug/$file -o $TESTDIR/$file.ll && \
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/malgo/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || echo 'FAIL!!'
done

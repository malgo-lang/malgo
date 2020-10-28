#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir $TESTDIR

for file in `ls ./examples/griff | grep Test`; do
  cabal exec griffc -- ./examples/griff/$file -o $TESTDIR/$file.ll && \
  clang -lgc ./runtime/griff/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out && \
  $TESTDIR/$file.out || exit 255
  echo 'SUCCESS!!'
done

#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir $TESTDIR
mkdir $TESTDIR/libs

BUILD=cabal

eval "$BUILD exec malgo -- to-ll --force ./runtime/malgo/Builtin.mlg -M $TESTDIR/libs -o $TESTDIR/libs/Builtin.ll || exit 255"
eval "$BUILD exec malgo -- to-ll --force ./runtime/malgo/Prelude.mlg -M $TESTDIR/libs -o $TESTDIR/libs/Prelude.ll || exit 255"
cp ./runtime/malgo/runtime.c $TESTDIR/libs/runtime.c

echo '=== testcases/malgo/* ==='
for file in `ls ./testcases/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./testcases/malgo/$file -o $LLFILE || echo 'FAIL'"
done

echo '=== examples/malgo/* ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./examples/malgo/$file -o $LLFILE || echo 'FAIL'"
done

#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir -p $TESTDIR
mkdir -p $TESTDIR/libs

BUILD=cabal

eval "$BUILD exec malgo -- to-ll -M $TESTDIR/libs ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll || exit 255"
eval "$BUILD exec malgo -- to-ll -M $TESTDIR/libs ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll || exit 255"
cp ./runtime/malgo/runtime.c $TESTDIR/libs/runtime.c

echo '=== via llvm-hs ==='
for file in `ls ./testcases/malgo/error | grep '\.mlg$'`; do
  echo "--- testcases/malgo/error/$file ---"

  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  eval "$BUILD exec malgo -- to-ll -M $TESTDIR/libs ./testcases/malgo/error/$file -o $LLFILE"
  if [ $? -eq 0 ]; then
    echo fail
    exit 1
  fi
done

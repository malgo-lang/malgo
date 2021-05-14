#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir $TESTDIR
mkdir $TESTDIR/libs

BUILD=cabal

# eval "$BUILD exec malgo -- to-ll --force ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll || exit 255"
# eval "$BUILD exec malgo -- to-ll --force ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll || exit 255"
cp ./runtime/malgo/rts.c $TESTDIR/libs/rts.c

echo '=== via llvm-hs (with core json) ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  # eval "$BUILD exec malgo -- to-ll --force ./examples/malgo/$file -o $LLFILE || exit 255"
  eval "$BUILD exec malgo -- to-ll --force ./examples/malgo/$file -o $LLFILE || echo 'FAIL'"

  # clang -O2 -flto $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255
done

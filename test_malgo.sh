#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir $TESTDIR
mkdir $TESTDIR/libs

BUILD=cabal

eval "$BUILD exec malgoc -- to-ll --force --via-binding ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll || exit 255"
eval "$BUILD exec malgoc -- to-ll --force --via-binding ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll || exit 255"
cp ./runtime/malgo/rts.c $TESTDIR/libs/rts.c

echo '=== no opt no lambdalift ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force --no-opt --no-lambdalift ./examples/malgo/$file -o $LLFILE || exit 255"

  clang $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

echo '=== no opt ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force --no-opt ./examples/malgo/$file -o $LLFILE || exit 255"

  clang $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

echo '=== no lambdalift ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force --no-lambdalift ./examples/malgo/$file -o $LLFILE || exit 255"

  clang $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

echo '=== opt ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force ./examples/malgo/$file -o $LLFILE || exit 255"

  clang -O2 $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

echo '=== via llvm-hs (with core json) ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force --via-binding --gen-core-json ./examples/malgo/$file -o $LLFILE || exit 255"

  clang -O2 -flto $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

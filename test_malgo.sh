#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir $TESTDIR
mkdir $TESTDIR/libs

BUILD=cabal

set -x

eval "$BUILD exec malgoc -- --via-binding ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll || exit 255"
eval "$BUILD exec malgoc -- --via-binding ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll || exit 255"
clang $(pkg-config bdw-gc --cflags) -Wall -Wextra -O2 -S -emit-llvm ./runtime/malgo/rts.c -o $TESTDIR/libs/rts.ll || exit 255

echo '=== no opt no lambdalift ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  echo "# $file"
  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- --no-opt --no-lambdalift -M $TESTDIR/libs ./examples/malgo/$file -o $LLFILE || exit 255"

  clang $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.ll $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

echo '=== no opt ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  echo "# $file"
  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- --no-opt -M $TESTDIR/libs ./examples/malgo/$file -o $LLFILE || exit 255"

  clang $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.ll $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

echo '=== no lambdalift ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  echo "# $file"
  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- --no-lambdalift -M $TESTDIR/libs ./examples/malgo/$file -o $LLFILE || exit 255"

  clang $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.ll $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

echo '=== opt ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  echo "# $file"
  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- -M $TESTDIR/libs ./examples/malgo/$file -o $LLFILE || exit 255"

  clang -O2 $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.ll $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

echo '=== via llvm-hs (with core json) ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  echo "# $file"
  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- --via-binding --gen-core-json -M $TESTDIR/libs ./examples/malgo/$file -o $LLFILE || exit 255"

  clang -O2 -flto $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.ll $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255

  test "$($OUTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

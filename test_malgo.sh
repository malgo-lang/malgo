#!/usr/bin/env bash

TESTDIR=/tmp/malgo_test
mkdir $TESTDIR
mkdir $TESTDIR/libs

BUILD=cabal

eval "$BUILD exec malgoc -- to-ll --force ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll || exit 255"
eval "$BUILD exec malgoc -- to-ll --force ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll || exit 255"
cp ./runtime/malgo/rts.c $TESTDIR/libs/rts.c

echo '=== no opt no lambdalift ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-nono}
  OPTFILE=$TESTDIR/${file/.mlg/.opt-nono}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force --no-opt --no-lambdalift ./examples/malgo/$file -o $LLFILE || exit 255"

  clang -O0 $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  clang -O2 -flto $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OPTFILE || exit 255

  test "$($OPTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

echo '=== no opt ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-noopt}
  OPTFILE=$TESTDIR/${file/.mlg/.opt-noopt}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force --no-opt ./examples/malgo/$file -o $LLFILE || exit 255"

  clang -O0 $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  clang -O2 -flto $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OPTFILE || exit 255

  test "$($OPTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

echo '=== no lambdalift ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-nolift}
  OPTFILE=$TESTDIR/${file/.mlg/.opt-nolift}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force --no-lambdalift ./examples/malgo/$file -o $LLFILE || exit 255"

  clang -O0 $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  clang -O2 -flto $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OPTFILE || exit 255

  test "$($OPTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

echo '=== opt ==='
for file in `ls ./examples/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}
  OPTFILE=$TESTDIR/${file/.mlg/.opt}

  cat ./examples/malgo/$file | grep -q '^-- Expected: ' || exit 255

  eval "$BUILD exec malgoc -- to-ll --force ./examples/malgo/$file -o $LLFILE || exit 255"

  clang -O0 $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  clang -O2 -flto $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/rts.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OPTFILE || exit 255

  test "$($OPTFILE)" = "$(cat ./examples/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
done

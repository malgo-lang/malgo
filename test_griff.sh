#!/usr/bin/env bash

TESTDIR=/tmp/griff_test
mkdir $TESTDIR
mkdir $TESTDIR/libs

cabal exec griffc -- --via-binding ./runtime/griff/Builtin.grf -o $TESTDIR/Builtin.ll || exit 255
cabal exec griffc -- --via-binding ./runtime/griff/Prelude.grf -o $TESTDIR/Prelude.ll || exit 255

echo '=== no opt no lambdalift ==='
for file in `ls ./examples/griff | grep '\.grf$'`; do
  echo "# $file"
  LLFILE=$TESTDIR/${file/.grf/.ll}
  OUTFILE=$TESTDIR/${file/.grf/.out}
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- --no-opt --no-lambdalift ./examples/griff/$file -o $LLFILE || exit 255
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/Prelude.ll $TESTDIR/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  $OUTFILE || exit 255
  test "$($OUTFILE)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

echo '=== no opt ==='
for file in `ls ./examples/griff | grep '\.grf$'`; do
  echo "# $file"
  LLFILE=$TESTDIR/${file/.grf/.ll}
  OUTFILE=$TESTDIR/${file/.grf/.out}
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- --no-opt ./examples/griff/$file -o $LLFILE || exit 255
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/Prelude.ll $TESTDIR/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  $OUTFILE || exit 255
  test "$($OUTFILE)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

echo '=== no lambdalift ==='
for file in `ls ./examples/griff | grep '\.grf$'`; do
  echo "# $file"
  LLFILE=$TESTDIR/${file/.grf/.ll}
  OUTFILE=$TESTDIR/${file/.grf/.out}
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- --no-lambdalift ./examples/griff/$file -o $LLFILE || exit 255
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/Prelude.ll $TESTDIR/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  $OUTFILE || exit 255
  test "$($OUTFILE)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

echo '=== opt ==='
for file in `ls ./examples/griff | grep '\.grf$'`; do
  echo "# $file"
  LLFILE=$TESTDIR/${file/.grf/.ll}
  OUTFILE=$TESTDIR/${file/.grf/.out}
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- ./examples/griff/$file -o $LLFILE || exit 255
  clang -O2 $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/Prelude.ll $TESTDIR/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  $OUTFILE || exit 255
  test "$($OUTFILE)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

echo '=== via llvm-hs ==='
for file in `ls ./examples/griff | grep '\.grf$'`; do
  echo "# $file"
  LLFILE=$TESTDIR/${file/.grf/.ll}
  OUTFILE=$TESTDIR/${file/.grf/.out}
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- --via-binding ./examples/griff/$file -o $LLFILE || exit 255
  clang -O2 $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/Prelude.ll $TESTDIR/Builtin.ll $LLFILE -o $OUTFILE || exit 255
  $OUTFILE || exit 255
  test "$($OUTFILE)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo ""
done

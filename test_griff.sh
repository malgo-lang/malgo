#!/usr/bin/env bash

TESTDIR=/tmp/griff_test
mkdir $TESTDIR

echo '=== no opt no lambdalift ==='
for file in `ls ./examples/griff`; do
  echo $file;
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- --no-opt --no-lambdalift ./examples/griff/$file -o $TESTDIR/$file.ll || exit 255
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out || exit 255
  $TESTDIR/$file.out || exit 255
  test "$($TESTDIR/$file.out)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo 'SUCCESS!!'
done

echo '=== no opt ==='
for file in `ls ./examples/griff`; do
  echo $file;
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- --no-opt ./examples/griff/$file -o $TESTDIR/$file.ll || exit 255
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out || exit 255
  $TESTDIR/$file.out || exit 255
  test "$($TESTDIR/$file.out)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo 'SUCCESS!!'
done

echo '=== no lambdalift ==='
for file in `ls ./examples/griff`; do
  echo $file;
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- --no-lambdalift ./examples/griff/$file -o $TESTDIR/$file.ll || exit 255
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out || exit 255
  $TESTDIR/$file.out || exit 255
  test "$($TESTDIR/$file.out)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo 'SUCCESS!!'
done

echo '=== opt ==='
for file in `ls ./examples/griff`; do
  echo $file;
  cat ./examples/griff/$file | grep -q '^-- Expected: ' || exit 255
  cabal exec griffc -- ./examples/griff/$file -o $TESTDIR/$file.ll || exit 255
  clang $(pkg-config bdw-gc --libs --cflags) ./runtime/griff/rts.c $TESTDIR/$file.ll -o $TESTDIR/$file.out || exit 255
  $TESTDIR/$file.out || exit 255
  test "$($TESTDIR/$file.out)" = "$(cat ./examples/griff/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')" || exit 255
  echo 'SUCCESS!!'
done

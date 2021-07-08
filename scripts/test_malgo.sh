#!/usr/bin/env bash

set -eu

TESTDIR=/tmp/malgo_test
mkdir -p $TESTDIR
mkdir -p $TESTDIR/libs

BUILD=cabal

eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll"
eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll"
cp ./runtime/malgo/runtime.c $TESTDIR/libs/runtime.c

echo '=== no opt no lambdalift ==='
for file in `ls ./testcases/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-nono}
  OPTFILE=$TESTDIR/${file/.mlg/.opt-nono}

  echo $file

  cat ./testcases/malgo/$file | grep -q '^-- Expected: '

  eval "$BUILD exec malgo -- to-ll --force --no-opt --no-lambdalift -M $TESTDIR/libs ./testcases/malgo/$file -o $LLFILE"

  clang -Wno-override-module -O0 -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE
  clang -Wno-override-module -O3 -flto -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OPTFILE

  test "$(echo 'Hello' | $OPTFILE)" = "$(cat ./testcases/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"
  echo 'Hello' | $OPTFILE
  echo ''
done

echo '=== no opt ==='
for file in `ls ./testcases/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-noopt}
  OPTFILE=$TESTDIR/${file/.mlg/.opt-noopt}

  echo $file

  cat ./testcases/malgo/$file | grep -q '^-- Expected: '

  eval "$BUILD exec malgo -- to-ll --force --no-opt -M $TESTDIR/libs ./testcases/malgo/$file -o $LLFILE"

  clang -Wno-override-module -O0 -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE
  clang -Wno-override-module -O3 -flto -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OPTFILE

  test "$(echo 'Hello' | $OPTFILE)" = "$(cat ./testcases/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"
done

echo '=== no lambdalift ==='
for file in `ls ./testcases/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-nolift}
  OPTFILE=$TESTDIR/${file/.mlg/.opt-nolift}

  echo $file

  cat ./testcases/malgo/$file | grep -q '^-- Expected: '

  eval "$BUILD exec malgo -- to-ll --force --no-lambdalift -M $TESTDIR/libs ./testcases/malgo/$file -o $LLFILE"

  clang -Wno-override-module -O0 -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE
  clang -Wno-override-module -O3 -flto -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OPTFILE

  test "$(echo 'Hello' | $OPTFILE)" = "$(cat ./testcases/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"
done

echo '=== opt ==='
for file in `ls ./testcases/malgo | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}
  OPTFILE=$TESTDIR/${file/.mlg/.opt}

  echo $file

  cat ./testcases/malgo/$file | grep -q '^-- Expected: '

  eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./testcases/malgo/$file -o $LLFILE"

  clang -Wno-override-module -O0 -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE
  clang -Wno-override-module -O3 -flto -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OPTFILE

  test "$(echo 'Hello' | $OPTFILE)" = "$(cat ./testcases/malgo/$file | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"
done

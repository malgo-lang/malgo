#!/usr/bin/env bash

set -eu

TESTDIR=/tmp/malgo_test
mkdir -p $TESTDIR
mkdir -p $TESTDIR/libs

BUILD=stack

eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll"
eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll"
cp ./runtime/malgo/runtime.c $TESTDIR/libs/runtime.c

echo '=== no opt no lambdalift ==='
for file in `ls ./bench | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-nono}

  echo $file

  eval "$BUILD exec malgo -- to-ll --force --no-opt --no-lambdalift -M $TESTDIR/libs ./bench/$file -o $LLFILE"

  clang -O3 -flto -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE

  hyperfine --warmup 3 $OUTFILE
done

echo '=== no opt ==='
for file in `ls ./bench | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-noopt}

  echo $file

  eval "$BUILD exec malgo -- to-ll --force --no-opt -M $TESTDIR/libs ./bench/$file -o $LLFILE"

  clang -O3 -flto -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE

  hyperfine --warmup 3 $OUTFILE
done

echo '=== no lambdalift ==='
for file in `ls ./bench | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out-nolift}

  echo $file

  eval "$BUILD exec malgo -- to-ll --force --no-lambdalift -M $TESTDIR/libs ./bench/$file -o $LLFILE"

  clang -O3 -flto -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE

  hyperfine --warmup 3 $OUTFILE
done

echo '=== opt ==='
for file in `ls ./bench | grep '\.mlg$'`; do
  LLFILE=$TESTDIR/${file/.mlg/.ll}
  OUTFILE=$TESTDIR/${file/.mlg/.out}

  echo $file

  eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./bench/$file -o $LLFILE"

  clang -O3 -flto -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE

  hyperfine --warmup 3 $OUTFILE
done

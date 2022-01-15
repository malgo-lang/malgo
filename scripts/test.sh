#!/usr/bin/env bash

# 特定のファイルのみをテストする

set -eu

TESTDIR=/tmp/malgo_test
mkdir -p $TESTDIR
mkdir -p $TESTDIR/libs

BUILD=stack

TestFilePath=$1
file=`basename $TestFilePath`
malgoOptions=${@:2}

# 並列にテストを実行すると、ここでコンパイル順序の前提が崩れうる。
# コンパイル順序を保証するために、事前にpretest.shを実行する。
# eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll"
# eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll"
# cp ./runtime/malgo/runtime.c $TESTDIR/libs/runtime.c

echo '=== no opt no lambdalift ==='

LLFILE=$TESTDIR/${file/.mlg/.ll}
OUTFILE=$TESTDIR/${file/.mlg/.out-nono}

echo $file

cat $TestFilePath | grep -q '^-- Expected: '

eval "$BUILD exec malgo -- to-ll --force --no-opt --no-lambdalift -M $TESTDIR/libs $TestFilePath -o $LLFILE $malgoOptions"

clang -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE

test "$(echo 'Hello' | $OUTFILE)" = "$(cat $TestFilePath | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"


echo '=== no opt ==='

LLFILE=$TESTDIR/${file/.mlg/.ll}
OUTFILE=$TESTDIR/${file/.mlg/.out-noopt}

echo $file

cat $TestFilePath | grep -q '^-- Expected: '

eval "$BUILD exec malgo -- to-ll --force --no-opt -M $TESTDIR/libs $TestFilePath -o $LLFILE $malgoOptions"

clang -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE

test "$(echo 'Hello' | $OUTFILE)" = "$(cat $TestFilePath | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"


echo '=== no lambdalift ==='

LLFILE=$TESTDIR/${file/.mlg/.ll}
OUTFILE=$TESTDIR/${file/.mlg/.out-nolift}

echo $file

cat $TestFilePath | grep -q '^-- Expected: '

eval "$BUILD exec malgo -- to-ll --force --no-lambdalift -M $TESTDIR/libs $TestFilePath -o $LLFILE $malgoOptions"

clang -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE

test "$(echo 'Hello' | $OUTFILE)" = "$(cat $TestFilePath | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"


echo '=== opt ==='

LLFILE=$TESTDIR/${file/.mlg/.ll}
OUTFILE=$TESTDIR/${file/.mlg/.out}

echo $file

cat $TestFilePath | grep -q '^-- Expected: '

eval "$BUILD exec malgo -- to-ll --force -M $TESTDIR/libs $TestFilePath -o $LLFILE $malgoOptions"

clang -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $TESTDIR/libs/Prelude.ll $TESTDIR/libs/Builtin.ll $LLFILE -o $OUTFILE

test "$(echo 'Hello' | $OUTFILE)" = "$(cat $TestFilePath | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"

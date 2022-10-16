#!/usr/bin/env bash

# 特定のファイルのみをテストする（lamdba-liftをしない）

set -eu

TESTDIR=/tmp/malgo_test
mkdir -p $TESTDIR
mkdir -p $TESTDIR/libs

BUILD=''
# $2 is the string "stack" or "cabal"
# It is used to determine which command to use to test (assign to BUILD)
if [ "$#" = "2" ]; then
    if [ "$2" = "stack" ]; then
        BUILD="stack"
    fi
fi
if [ -z $BUILD ]; then
    BUILD="cabal"
fi

TestFilePath=$1
file=`basename $TestFilePath`
malgoOptions='' # No options

echo '=== no lambdalift ==='

LLFILE=$TESTDIR/${file/.mlg/-nolift.ll}
OUTFILE=$TESTDIR/${file/.mlg/-nolift.out}

echo $file

cat $TestFilePath | grep -q '^-- Expected: '

eval "$BUILD exec malgo -- to-ll -M $TESTDIR/libs $TestFilePath -o $LLFILE $malgoOptions"

clang -Wno-override-module -lm $(pkg-config bdw-gc --libs --cflags) $TESTDIR/libs/runtime.c $LLFILE -o $OUTFILE

test "$(echo 'Hello' | $OUTFILE)" = "$(cat $TestFilePath | grep '^-- Expected: ' | sed -e 's/^-- Expected: //')"

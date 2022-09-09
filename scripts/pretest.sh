#!/usr/bin/env bash

set -eu

TESTDIR=/tmp/malgo_test
mkdir -p $TESTDIR
mkdir -p $TESTDIR/libs

# $1 is the string "stack" or "cabal"
# It is used to determine which command to use to test (assign to BUILD)
BUILD="cabal"
if [ "$#" = "1" ]; then
    if [ "$1" = "stack" ]; then
        BUILD="stack"
    fi
fi

eval "$BUILD build"

eval "$BUILD exec malgo -- to-ll -M $TESTDIR/libs ./runtime/malgo/Builtin.mlg -o $TESTDIR/libs/Builtin.ll"
ls /tmp/malgo_test/libs/
eval "$BUILD exec malgo -- to-ll -M $TESTDIR/libs ./runtime/malgo/Prelude.mlg -o $TESTDIR/libs/Prelude.ll"
ls /tmp/malgo_test/libs/
cp ./runtime/malgo/runtime.c $TESTDIR/libs/runtime.c

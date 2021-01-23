#!/usr/bin/env bash

if [ -z "$XDG_DATA_HOME" ]; then
  LIB_PATH=$HOME/.local/share/malgo/base
else
  LIB_PATH=$XDG_DATA_HOME/malgo/base
fi

mkdir -p $LIB_PATH

cp runtime/malgo/rts.c $LIB_PATH/rts.c
cp runtime/malgo/Builtin.grf $LIB_PATH/Builtin.grf
cp runtime/malgo/Prelude.grf $LIB_PATH/Prelude.grf

cabal exec malgoc -- --via-binding $LIB_PATH/Builtin.grf
cabal exec malgoc -- --via-binding $LIB_PATH/Prelude.grf

clang -S -emit-llvm -O2 $LIB_PATH/Builtin.ll -o $LIB_PATH/Builtin.ll
clang -S -emit-llvm -O2 $LIB_PATH/Prelude.ll -o $LIB_PATH/Prelude.ll
clang $(pkg-config bdw-gc --cflags) -S -emit-llvm -O2 $LIB_PATH/rts.c -o $LIB_PATH/rts.ll
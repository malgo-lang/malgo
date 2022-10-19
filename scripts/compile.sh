#!/usr/bin/env bash

if [ -z "$XDG_DATA_HOME" ]; then
  LIB_PATH=$HOME/.local/share/malgo/base
else
  LIB_PATH=$XDG_DATA_HOME/malgo/base
fi

src_file=$(basename -- "$1")
malgoOptions=${@:2}

mkdir -p .malgo-work/build

malgo to-ll $LIB_PATH/Builtin.mlg -o .malgo-work/build/Builtin.ll

malgo to-ll $LIB_PATH/Prelude.mlg -o .malgo-work/build/Prelude.ll

malgo to-ll $1 -o .malgo-work/build/${src_file%.mlg}.ll $malgoOptions

clang -O3 -flto -lm $(pkg-config --cflags --libs bdw-gc) $LIB_PATH/runtime.c .malgo-work/build/${src_file%.mlg}.ll -o ${src_file%.mlg}

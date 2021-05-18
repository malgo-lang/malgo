#!/usr/bin/env bash

if [ -z "$XDG_DATA_HOME" ]; then
  LIB_PATH=$HOME/.local/share/malgo/base
else
  LIB_PATH=$XDG_DATA_HOME/malgo/base
fi

src_file=$(basename -- "$1")

malgo to-ll $1 -o .malgo-work/build/${src_file%.mlg}.ll

clang -O3 -flto $(pkg-config --cflags --libs bdw-gc) .malgo-work/build/Builtin.ll .malgo-work/build/Prelude.ll $LIB_PATH/rts.c .malgo-work/build/${src_file%.mlg}.ll -o ${src_file%.mlg}

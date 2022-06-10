#!/usr/bin/env bash

if [ -z "$XDG_DATA_HOME" ]; then
  LIB_PATH=$HOME/.local/share/malgo/base
else
  LIB_PATH=$XDG_DATA_HOME/malgo/base
fi

src_file=$(basename -- "$1")
malgoOptions=${@:2}

mkdir -p .malgo-work/build

bench "malgo to-ll $LIB_PATH/Builtin.mlg -o .malgo-work/build/Builtin.ll"

bench "malgo to-ll $LIB_PATH/Prelude.mlg -o .malgo-work/build/Prelude.ll"

bench "malgo to-ll $1 -o .malgo-work/build/${src_file%.mlg}.ll $malgoOptions"

echo "-O2"
bench "clang -O2 -lm $(pkg-config --cflags --libs bdw-gc) .malgo-work/build/Builtin.ll .malgo-work/build/Prelude.ll $LIB_PATH/runtime.c .malgo-work/build/${src_file%.mlg}.ll -o ${src_file%.mlg}"
bench "./${src_file%.mlg}"

echo "-O2 -flto"
bench "clang -O2 -flto -lm $(pkg-config --cflags --libs bdw-gc) .malgo-work/build/Builtin.ll .malgo-work/build/Prelude.ll $LIB_PATH/runtime.c .malgo-work/build/${src_file%.mlg}.ll -o ${src_file%.mlg}"
bench "./${src_file%.mlg}"

echo "-O3"
bench "clang -O3 -lm $(pkg-config --cflags --libs bdw-gc) .malgo-work/build/Builtin.ll .malgo-work/build/Prelude.ll $LIB_PATH/runtime.c .malgo-work/build/${src_file%.mlg}.ll -o ${src_file%.mlg}"
bench "./${src_file%.mlg}"

echo "-O3 -flto"
bench "clang -O3 -flto -lm $(pkg-config --cflags --libs bdw-gc) .malgo-work/build/Builtin.ll .malgo-work/build/Prelude.ll $LIB_PATH/runtime.c .malgo-work/build/${src_file%.mlg}.ll -o ${src_file%.mlg}"
bench "./${src_file%.mlg}"

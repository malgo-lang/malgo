#!/usr/bin/env bash

if [ -z "$XDG_DATA_HOME" ]; then
  LIB_PATH=$HOME/.local/share/griff/base
else
  LIB_PATH=$XDG_DATA_HOME/griff/base
fi

mkdir -p $LIB_PATH

cp runtime/griff/rts.c $LIB_PATH/rts.c
cp runtime/griff/Builtin.grf $LIB_PATH/Builtin.grf
cp runtime/griff/Prelude.grf $LIB_PATH/Prelude.grf

cabal exec griffc -- --via-binding $LIB_PATH/Builtin.grf
cabal exec griffc -- --via-binding $LIB_PATH/Prelude.grf
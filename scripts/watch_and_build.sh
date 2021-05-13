#!/usr/bin/env bash

cat <(find koriel/src -name \*.hs) <(find malgo/src -name \*.hs) <(find malgo/app -name \*.hs) | entr sh -c 'hpack koriel; hpack malgo; cabal build all; echo DONE'

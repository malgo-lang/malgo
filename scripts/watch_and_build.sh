#!/usr/bin/env bash

cat <(echo 'malgo/package.yaml') <(echo 'koriel/package.yaml') <(find koriel/src -name \*.hs) <(find malgo/src -name \*.hs) <(find malgo/app -name \*.hs) | entr sh -c 'hpack koriel; hpack malgo; cabal build all; hlint koriel; hlint malgo; echo DONE'

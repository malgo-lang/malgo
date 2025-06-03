#!/bin/zsh

grep '^import' src/Malgo/**/*.hs | python graph.py > dependencies.dot
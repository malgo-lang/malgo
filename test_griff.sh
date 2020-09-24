#!/usr/bin/env bash

stack exec griff -- examples/griff/Test0.grf
clang out.ll -lgc examples/griff/lib.c
stack exec griff -- examples/griff/Test1.grf
clang out.ll -lgc examples/griff/lib.c
stack exec griff -- examples/griff/Test2.grf
clang out.ll -lgc examples/griff/lib.c
stack exec griff -- examples/griff/Test3.grf
clang out.ll -lgc examples/griff/lib.c
stack exec griff -- examples/griff/Test4.grf
clang out.ll -lgc examples/griff/lib.c
stack exec griff -- examples/griff/Test5.grf
clang out.ll -lgc examples/griff/lib.c
stack exec griff -- examples/griff/Test6.grf
clang out.ll -lgc examples/griff/lib.c
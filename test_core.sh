#!/usr/bin/env bash

# testcasesがコンパイルできるかチェック
mkdir ./tmp
stack build
ls ./testcases | grep mlg | xargs -I{} sh -c 'echo {} && stack exec malgo -- --core-mode ./testcases/{} -o ./tmp/{}.ll && clang -lgc ./examples/corelib.c ./tmp/{}.ll -o ./tmp/{}.out && ./tmp/{}.out || exit 255' && rm ./tmp/*.ll && rm ./tmp/*.out

ls ./testcases/bug | grep mlg | xargs -I{} sh -c 'echo {} && stack exec malgo -- --core-mode ./testcases/bug/{} -o ./tmp/{}.ll && clang -lgc ./examples/corelib.c ./tmp/{}.ll -o ./tmp/{}.out && ./tmp/{}.out'

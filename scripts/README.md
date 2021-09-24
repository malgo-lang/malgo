# Shell scripts for malgo

* [bench.sh](bench.sh) : Benchmark for programs written in Malgo
* [build.sh](build.sh) : Build malgo compiler using cabal
* [compile.sh](compile.sh) : Compile and make executable from a source program
* [install_malgo_internal.sh](install_malgo_internal.sh) : Install malgo base library to $XDG_DATA_HOME/malgo/base
* [test.sh](test.sh) : Run a single test
* [test_malgo.sh](test_malgo.sh) : Run test which will be successfully compiled
* [test_malgo_error.sh](test_malgo_error.sh) : Run test which will throw compile error
* [test_malgo_only_compile.sh](test_malgo_only_compile.sh) : Run test as [test_malgo.sh](test_malgo.sh) but only .mlg to .ll pass
* [test_malgo_parallel.sh](test_malgo_parallel.sh) : Run tests in parallel
* [watch_and_build.sh](watch_and_build.sh) : Watch file updates and run [build.sh](build.sh) using [entr](https://github.com/eradman/entr)

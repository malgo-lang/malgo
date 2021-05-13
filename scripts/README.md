# Shell scripts for malgo

* [build.sh](scripts/build.sh) : Build malgo compiler using cabal
* [watch_and_build.sh](scripts/watch_and_build.sh) : Watch file updates and run [build.sh](scripts/build.sh) using [entr](https://github.com/eradman/entr)
* [install_malgo_internal.sh](scripts/install_malgo_internal.sh) : Install malgo base library to $XDG_DATA_HOME/malgo/base
* [test_malgo.sh](scripts/test_malgo.sh) : Run test which will be successfully compiled
* [test_malgo_error.sh](scripts/test_malgo_error.sh) : Run test which will throw compile error
* [test_malgo_only_compile.sh](scripts/test_malgo_only_compile.sh) : Run test as [test_malgo.sh](scripts/build.sh) but only .mlg to .ll pass


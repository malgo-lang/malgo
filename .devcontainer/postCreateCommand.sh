ghcup install stack 2.11.1
ghcup set stack 2.11.1

ghcup compile hls --version 2.1.0.0 --ghc 9.6.2 -j$(nproc)

cabal install ghc-prof-flamegraph --ignore-project
cabal install hlint --ignore-project
cabal install ghcid --ignore-project
cabal install implicit-hie --ignore-project

cabal install hoogle --ignore-project
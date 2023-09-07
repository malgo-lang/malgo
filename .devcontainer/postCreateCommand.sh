ghcup install stack 2.11.1
ghcup set stack 2.11.1

ghcup compile hls --git-ref 9871ecbde48f8e2fd328646d5149d28076e36711 --ghc 9.6.2 -j$(nproc)

cabal install ghc-prof-flamegraph --ignore-project
cabal install hlint --ignore-project
cabal install ghcid --ignore-project
cabal install implicit-hie --ignore-project

cabal install hoogle --ignore-project
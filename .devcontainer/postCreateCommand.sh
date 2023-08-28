ghcup install stack 2.11.1
ghcup set stack 2.11.1

ghcup compile hls --version 2.1.0.0 --ghc 9.6.2 -j$(nproc)

stack install ghc-prof-flamegraph
stack install hlint
stack install ghcid
stack install implicit-hie
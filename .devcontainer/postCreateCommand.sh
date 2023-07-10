# ghcup compile hls --git-ref 7860df3e97ec5a286697aeed3f05a9ab08e7617e --ghc 9.6.2 -j$(nproc)
ghcup install hls
stack install ghc-prof-flamegraph # --ignore-project
stack install hlint # --ignore-project
stack install ghcid # --ignore-project
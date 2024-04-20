# ghcup compile hls --version 2.1.0.0 --ghc 9.6.2 -j$(nproc)
# ghcup install hls
stack install ghc-prof-flamegraph # --ignore-project
stack install hlint # --ignore-project
stack install ghcid # --ignore-project

cargo install git-cliff
ghcup compile hls --git-ref 90d71cee120bd50473098c8522820fce4418ede5 --ghc 9.6.2 -j$(nproc)
# ghcup install hls
stack install ghc-prof-flamegraph # --ignore-project
stack install hlint # --ignore-project
stack install ghcid # --ignore-project
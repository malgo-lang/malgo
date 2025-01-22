/home/vscode/.local/bin/mise trust && /home/vscode/.local/bin/mise install && /home/vscode/.local/bin/mise run setup

cabal update && cabal install hpack

ghcup compile hls --git-ref master --ghc 9.6.4 -j 4
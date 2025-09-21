/home/vscode/.local/bin/mise trust
/home/vscode/.local/bin/mise install
# /home/vscode/.local/bin/mise run setup
/home/vscode/.local/share/mise/installs/ghcup/0.1.50.2/ghcup install ghc 9.12.2
/home/vscode/.local/share/mise/installs/ghcup/0.1.50.2/ghcup set ghc 9.12.2
/home/vscode/.local/share/mise/installs/ghcup/0.1.50.2/ghcup install cabal latest
/home/vscode/.local/share/mise/installs/ghcup/0.1.50.2/ghcup set cabal latest
cabal update
cabal install hpack --overwrite-policy=always
cabal install ormolu --overwrite-policy=always
# /home/vscode/.local/bin/mise run setup-hls
ghcup compile hls -g 2.11.0.0 --ghc 9.12.2 --cabal-update
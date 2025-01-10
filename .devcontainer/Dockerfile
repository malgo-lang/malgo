FROM mcr.microsoft.com/devcontainers/base:ubuntu

RUN apt-get update && apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

USER vscode

# install haskell

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1

ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.10.1
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.12.1.0
# Disable HLS installation
# ENV BOOTSTRAP_HASKELL_INSTALL_HLS=0
# ENV BOOTSTRAP_HASKELL_HLS_VERSION=2.9.0.1

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# install mise
RUN curl https://mise.run | sh

RUN echo 'eval "$(/home/vscode/.local/bin/mise activate bash)"' >> /home/vscode/.bashrc

# WORKDIR /workspace/malgo
# RUN /home/vscode/.ghcup/bin/cabal update
# RUN /home/vscode/.ghcup/bin/cabal install hpack
# RUN eval "$(/home/vscode/.local/bin/mise activate bash)"
# RUN /home/vscode/.local/bin/mise trust
# RUN /home/vscode/.local/bin/mise install
# RUN /home/vscode/.local/bin/mise run setup

# FROM haskell:9.2.2
ARG VARIANT=bullseye
FROM mcr.microsoft.com/vscode/devcontainers/base:${VARIANT}

WORKDIR /opt/malgo

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive && \
  apt-get install -y --no-install-recommends \
  lsb-release wget software-properties-common gnupg curl libgc-dev build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 zlib1g-dev && \
  apt-get clean && rm -rf /var/lib/apt/lists/*

RUN wget https://apt.llvm.org/llvm.sh && \
  chmod +x llvm.sh && \
  ./llvm.sh 12

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=${PATH}:/root/.ghcup/bin

RUN ghcup upgrade && \
  ghcup install ghc 9.2.2 && \
  ghcup set ghc 9.2.2 && \
  ghcup rm ghc 8.10.7 && \
  ghcup install hls

ENV PATH=${PATH}:/root/.cabal/bin

COPY ./malgo-compiler/malgo.cabal /opt/malgo/malgo-compiler/malgo.cabal
COPY ./cabal.project* /opt/malgo/

RUN cabal update && \
  cabal build --only-dependencies -j

FROM haskell:9.2.4

WORKDIR /opt/malgo

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive && \
  apt-get install -y --no-install-recommends \
  lsb-release wget software-properties-common gnupg curl libgc-dev build-essential curl libffi-dev libffi6 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 zlib1g-dev && \
  apt-get clean && rm -rf /var/lib/apt/lists/*

RUN wget https://apt.llvm.org/llvm.sh && \
  chmod +x llvm.sh && \
  ./llvm.sh 12 all

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=${PATH}:/root/.ghcup/bin

RUN ghcup upgrade && ghcup install ghc 9.2.4 && ghcup set ghc 9.2.4 && ghcup install hls

COPY ./malgo.cabal /opt/malgo/malgo.cabal
COPY ./cabal.project* /opt/malgo/

RUN cabal update && \
  cabal build --only-dependencies -j

RUN cabal install hpack hlint
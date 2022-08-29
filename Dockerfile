FROM haskell:9.2.2

WORKDIR /opt/malgo

RUN apt-get update
RUN apt-get install -y lsb-release wget software-properties-common gnupg curl

RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 12

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=${PATH}:/root/.ghcup/bin

RUN ghcup upgrade
RUN ghcup install ghc 9.2.2
RUN ghcup set ghc 9.2.2
RUN ghcup install hls

RUN cabal update

COPY ./malgo.cabal /opt/malgo/malgo.cabal
COPY ./cabal.project /opt/malgo/cabal.project
COPY ./cabal.project.local /opt/malgo/cabal.project.local

RUN cabal build --only-dependencies -j

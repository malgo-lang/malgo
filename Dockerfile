FROM buildpack-deps:stable

# https://askubuntu.com/questions/496549/error-you-must-put-some-source-uris-in-your-sources-list/857433#857433?newreg=efe9a80ed1fc48089f11ef95118b529b
# RUN cp /etc/apt/sources.list /etc/apt/sources.list~
# RUN sed -Ei 's/^# deb-src /deb-src /' /etc/apt/sources.list
# https://wiki.debian.org/SourcesList
RUN echo 'deb-src http://deb.debian.org/debian bullseye main' >> /etc/apt/sources.list
RUN echo 'deb-src http://deb.debian.org/debian-security/ bullseye-security main' >> /etc/apt/sources.list
RUN echo 'deb-src http://deb.debian.org/debian bullseye-updates main' >> /etc/apt/sources.list

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive && \
  # apt-get build-dep -y --no-install-recommends ghc && \
  apt-get install -y --no-install-recommends \
  lsb-release wget software-properties-common gnupg && \
  apt-get install -y --no-install-recommends \
  libgc-dev && \
  apt-get clean && rm -rf /var/lib/apt/lists/*

RUN wget https://apt.llvm.org/llvm.sh && \
  chmod +x llvm.sh && \
  ./llvm.sh 12

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=/root/.ghcup/bin:${PATH}

RUN ghcup upgrade && ghcup install ghc 9.2.4 && ghcup set ghc 9.2.4

# RUN ghcup compile hls --git-ref bc18cedf157e19c75c5676d8defcb982d6488fad --ghc 9.2.4
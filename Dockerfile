FROM buildpack-deps:stable AS ghcup-install

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=/root/.cabal/bin:/root/.ghcup/bin:${PATH}

RUN ghcup upgrade && ghcup install ghc 9.2.4 && ghcup set ghc 9.2.4

RUN ghcup compile hls --git-ref bc18cedf157e19c75c5676d8defcb982d6488fad --ghc 9.2.4

RUN ghcup gc

FROM buildpack-deps:stable

# https://wiki.debian.org/SourcesList
RUN echo 'deb-src http://deb.debian.org/debian bullseye main' >> /etc/apt/sources.list
RUN echo 'deb-src http://deb.debian.org/debian-security/ bullseye-security main' >> /etc/apt/sources.list
RUN echo 'deb-src http://deb.debian.org/debian bullseye-updates main' >> /etc/apt/sources.list

RUN apt-get update
ENV DEBIAN_FRONTEND noninteractive

# https://stackoverflow.com/questions/28405902/how-to-set-the-locale-inside-a-debian-ubuntu-docker-container
# https://qiita.com/suin/items/856bf782d0d295352e51
RUN apt-get install -y --no-install-recommends \
  locales
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8  

RUN apt-get install -y --no-install-recommends \
  lsb-release wget software-properties-common gnupg

RUN wget https://apt.llvm.org/llvm.sh && \
  chmod +x llvm.sh && \
  ./llvm.sh 12

RUN apt-get install -y --no-install-recommends \
  libgc-dev

RUN apt-get clean && rm -rf /var/lib/apt/lists/*

COPY --from=ghcup-install /root/.ghcup/ /root/.ghcup/

ENV PATH=/root/.cabal/bin:/root/.ghcup/bin:${PATH}

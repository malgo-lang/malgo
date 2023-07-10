FROM buildpack-deps:stable AS ghcup-install

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create the user
RUN groupadd --gid $USER_GID $USERNAME \
  && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
  #
  # [Optional] Add sudo support. Omit if you don't need to install software after connecting.
  && apt-get update \
  && apt-get install -y sudo \
  && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
  && chmod 0440 /etc/sudoers.d/$USERNAME

# ********************************************************
# * Anything else you want to do like clean up goes here *
# ********************************************************

# [Optional] Set the default user. Omit if you want to keep the default as root.
USER $USERNAME

ENV HOME=/home/$USERNAME

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.ghcup/bin:${PATH}

RUN echo $PATH && ghcup upgrade && ghcup install ghc 9.6.2 && ghcup set ghc 9.6.2

RUN ghcup install cabal 3.10.1.0 && ghcup set cabal 3.10.1.0 && cabal update

# RUN ghcup compile hls --git-ref 7860df3e97ec5a286697aeed3f05a9ab08e7617e --ghc 9.6.2 -j$(nproc)

RUN ghcup gc

WORKDIR $HOME

# RUN cabal install ghc-prof-flamegraph --ignore-project
# RUN cabal install hlint --ignore-project
# RUN cabal install ghcid --ignore-project

FROM buildpack-deps:stable

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create the user
RUN groupadd --gid $USER_GID $USERNAME \
  && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
  #
  # [Optional] Add sudo support. Omit if you don't need to install software after connecting.
  && apt-get update \
  && apt-get install -y sudo \
  && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
  && chmod 0440 /etc/sudoers.d/$USERNAME

# ********************************************************
# * Anything else you want to do like clean up goes here *
# ********************************************************

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

# [Optional] Set the default user. Omit if you want to keep the default as root.
USER $USERNAME

ENV HOME=/home/$USERNAME

ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8  

RUN sudo usermod --shell /bin/bash vscode

RUN sudo apt-get install -y --no-install-recommends \
  lsb-release wget software-properties-common gnupg cmake

WORKDIR $HOME/scripts

RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN sudo ./llvm.sh 15

RUN sudo apt-get install -y --no-install-recommends \
  libgc-dev

RUN sudo apt-get clean && sudo rm -rf /var/lib/apt/lists/*

COPY --from=ghcup-install $HOME/.ghcup/ $HOME/.ghcup/
COPY --from=ghcup-install $HOME/.cabal/ $HOME/.cabal/
COPY --from=ghcup-install $HOME/.stack/ $HOME/.stack/

ENV PATH=$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.ghcup/bin:${PATH}

RUN sudo chown -R $USERNAME:$USERNAME $HOME/.ghcup
RUN sudo chown -R $USERNAME:$USERNAME $HOME/.cabal
RUN sudo chown -R $USERNAME:$USERNAME $HOME/.stack
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

ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.6.2

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.ghcup/bin:${PATH}

RUN echo $PATH && ghcup upgrade && ghcup install ghc 9.6.2 && ghcup set ghc 9.6.2

RUN ghcup install cabal 3.10.1.0 && ghcup set cabal 3.10.1.0 && cabal update

RUN ghcup install stack 2.11.1
RUN ghcup set stack 2.11.1

# RUN ghcup compile hls --git-ref 9871ecbde48f8e2fd328646d5149d28076e36711 --ghc 9.6.2
RUN ghcup install hls 2.2.0.0

RUN ghcup gc

# RUN cabal install ghc-prof-flamegraph hlint ghcid implicit-hie hoogle --ignore-project

FROM buildpack-deps:stable AS go-install

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

WORKDIR /home/vscode/tmp

ARG TARGETPLATFORM

RUN if [ "$TARGETPLATFORM" = "linux/amd64" ]; then \
  wget https://go.dev/dl/go1.21.1.linux-amd64.tar.gz && sudo rm -rf /usr/local/go && sudo tar -C /usr/local -xzf go1.21.1.linux-amd64.tar.gz; \
  else \
  wget https://go.dev/dl/go1.21.1.linux-arm64.tar.gz && sudo rm -rf /usr/local/go && sudo tar -C /usr/local -xzf go1.21.1.linux-arm64.tar.gz; \
  fi

FROM buildpack-deps:stable AS final

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
  libicu-dev
#  lsb-release wget software-properties-common gnupg cmake

RUN sudo apt-get clean && sudo rm -rf /var/lib/apt/lists/*

COPY --from=ghcup-install $HOME/.ghcup/ $HOME/.ghcup/

COPY --from=ghcup-install $HOME/.cabal/ $HOME/.cabal/
COPY --from=ghcup-install $HOME/.stack/ $HOME/.stack/
COPY --from=go-install /usr/local/go/ /usr/local/go/

ENV PATH=$HOME/go/bin:/usr/local/go/bin:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.ghcup/bin:${PATH}

RUN sudo chown -R $USERNAME:$USERNAME $HOME/.ghcup
RUN sudo chown -R $USERNAME:$USERNAME $HOME/.cabal
RUN sudo chown -R $USERNAME:$USERNAME $HOME/.stack
RUN sudo chown -R $USERNAME:$USERNAME /usr/local/go
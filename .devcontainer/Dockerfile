FROM archlinux/base

# This Dockerfile adds a non-root user with sudo access. Use the "remoteUser"
# property in devcontainer.json to use it. On Linux, the container user's GID/UIDs
# will be updated to match your local UID/GID (when using the dockerFile property).
# See https://aka.ms/vscode-remote/containers/non-root-user for details.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid ${USER_GID} ${USERNAME}
RUN useradd --uid ${USER_UID} --gid ${USER_GID} -m ${USERNAME}
RUN pacman --noconfirm -Syu
RUN pacman --noconfirm -S base base-devel
RUN pacman --noconfirm -S sudo
RUN echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME}
RUN chmod 0440 /etc/sudoers.d/${USERNAME}

RUN pacman --noconfirm -S git

RUN runuser -l ${USERNAME} -c 'curl https://gitlab.haskell.org/haskell/ghcup/raw/master/bootstrap-haskell -sSf > install.sh'
RUN runuser -l ${USERNAME} -c 'chmod +x install.sh'
RUN runuser -l ${USERNAME} -c 'BOOTSTRAP_HASKELL_NONINTERACTIVE=true ./install.sh'
RUN runuser -l ${USERNAME} -c '. /home/vscode/.ghcup/env'
RUN runuser -l ${USERNAME} -c '. /home/vscode/.ghcup/env && ghcup install 8.8.2'
RUN runuser -l ${USERNAME} -c '. /home/vscode/.ghcup/env && ghcup set 8.8.2'
RUN runuser -l ${USERNAME} -c "echo '. /home/vscode/.ghcup/env' >> /home/vscode/.bashrc"

RUN runuser -l ${USERNAME} -c 'curl -sSL https://get.haskellstack.org/ | sh'
RUN runuser -l ${USERNAME} -c 'stack setup'

RUN runuser -l ${USERNAME} -c '. /home/vscode/.ghcup/env && cabal update'
RUN runuser -l ${USERNAME} -c 'stack update'

RUN runuser -l ${USERNAME} -c 'cd ~ && git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules'
RUN runuser -l ${USERNAME} -c '. /home/vscode/.ghcup/env && cd ~/haskell-ide-engine && ./cabal-hie-install hie-8.8.2'
RUN runuser -l ${USERNAME} -c '. /home/vscode/.ghcup/env && cd ~/haskell-ide-engine && ./cabal-hie-install data'

USER ${USERNAME}
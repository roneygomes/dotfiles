FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# System packages
RUN apt-get update && apt-get install -y \
    build-essential \
    ca-certificates gnupg \
    curl wget unzip \
    git \
    zsh tmux \
    ripgrep bat stow vim \
    jq \
    sudo \
    locales \
    && locale-gen en_US.UTF-8 \
    && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

# Node.js 24.x
RUN curl -fsSL https://deb.nodesource.com/setup_24.x | bash - \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# GitHub CLI
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    | gpg --dearmor -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
    > /etc/apt/sources.list.d/github-cli.list \
    && apt-get update && apt-get install -y gh \
    && rm -rf /var/lib/apt/lists/*

# Install global npm packages as root (writes to /usr/lib/node_modules)
RUN npm install -g @anthropic-ai/claude-code @openai/codex

# Ubuntu 24.04 ships an 'ubuntu' user at UID 1000; rename it to 'dev'
RUN usermod -l dev -d /home/dev -m -s /bin/zsh ubuntu \
    && groupmod -n dev ubuntu \
    && echo "dev ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/dev \
    && chmod 0440 /etc/sudoers.d/dev

USER dev
WORKDIR /home/dev

# Install oh-my-zsh
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended

# Install powerlevel10k theme + pre-download gitstatusd so the first shell start isn't slow
RUN git clone --depth=1 https://github.com/romkatv/powerlevel10k.git \
    ~/.oh-my-zsh/custom/themes/powerlevel10k \
    && ~/.oh-my-zsh/custom/themes/powerlevel10k/gitstatus/install

# Install fzf from source (apt package lacks key-bindings at the path oh-my-zsh expects)
RUN git clone --depth=1 https://github.com/junegunn/fzf.git ~/.fzf \
    && ~/.fzf/install --all --no-update-rc

# Bake in p10k config
COPY --chown=dev:dev .p10k.zsh /home/dev/.p10k.zsh

# Install atuin
RUN curl -sSf https://setup.atuin.sh | bash

ENV SHELL=/bin/zsh
ENV PATH="/home/dev/.atuin/bin:${PATH}"
ENV IN_CONTAINER=1
ENV POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

WORKDIR /home/dev/projects

CMD ["/bin/zsh"]

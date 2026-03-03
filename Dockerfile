FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# ── Bootstrap (bare minimum for nix to run) ───────────────────────────────────
RUN apt-get update && apt-get install -y \
    build-essential \
    ca-certificates \
    curl \
    git \
    locales \
    openssh-client \
    sudo \
    xz-utils \
    zsh \
    && locale-gen en_US.UTF-8 \
    && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# ── Docker CLI (DooD – daemon runs on host via mounted socket) ────────────────
RUN install -m 0755 -d /etc/apt/keyrings \
    && curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc \
    && chmod a+r /etc/apt/keyrings/docker.asc \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu $(. /etc/os-release && echo "$VERSION_CODENAME") stable" \
    > /etc/apt/sources.list.d/docker.list \
    && apt-get update && apt-get install -y \
    docker-ce-cli \
    docker-buildx-plugin \
    docker-compose-plugin \
    && rm -rf /var/lib/apt/lists/*

# ── Create dev user ───────────────────────────────────────────────────────────
# Ubuntu 24.04 ships an 'ubuntu' user at UID 1000; rename it to 'dev'
RUN usermod -l dev -d /home/dev -m -s /bin/zsh ubuntu \
    && groupmod -n dev ubuntu \
    && echo "dev ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/dev \
    && chmod 0440 /etc/sudoers.d/dev

RUN mkdir -p /nix && chown dev:dev /nix

USER dev
WORKDIR /home/dev

# ── Nix (single-user, no daemon) ──────────────────────────────────────────────
RUN curl -L https://nixos.org/nix/install | sh -s -- --no-daemon \
    && mkdir -p ~/.config/nix \
    && echo "experimental-features = nix-command flakes" > ~/.config/nix/nix.conf

ENV PATH=/home/dev/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH

# ── Dev tools via flake ───────────────────────────────────────────────────────
COPY --chown=dev:dev flake.nix /tmp/devenv/flake.nix
RUN nix profile install 'path:/tmp/devenv' \
    && nix-collect-garbage -d \
    && rm -rf /tmp/devenv

RUN git lfs install

# ── oh-my-zsh ─────────────────────────────────────────────────────────────────
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended

# ── Powerlevel10k + pre-download gitstatusd ───────────────────────────────────
RUN git clone --depth=1 https://github.com/romkatv/powerlevel10k.git \
    ~/.oh-my-zsh/custom/themes/powerlevel10k \
    && ~/.oh-my-zsh/custom/themes/powerlevel10k/gitstatus/install

# ── fzf from source (oh-my-zsh plugin needs ~/.fzf/shell/) ───────────────────
RUN git clone --depth=1 https://github.com/junegunn/fzf.git ~/.fzf \
    && ~/.fzf/install --all --no-update-rc

# ── nvm + Node LTS ───────────────────────────────────────────────────────────
# nvm needs bash and must not see the nix-provided node in PATH
SHELL ["/bin/bash", "-c"]
RUN export NVM_DIR="$HOME/.nvm" \
    && curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash \
    && source "$NVM_DIR/nvm.sh" \
    && nvm install --lts \
    && nvm alias default 'lts/*'
SHELL ["/bin/sh", "-c"]

# ── Entrypoint ────────────────────────────────────────────────────────────────
# Dotfiles are mounted as a volume at runtime (/home/dev/.dotfiles).
# The entrypoint symlinks them into place on each container start.
COPY --chown=dev:dev docker/entrypoint.sh /home/dev/.local/bin/entrypoint.sh
RUN chmod +x /home/dev/.local/bin/entrypoint.sh

ENV SHELL=/bin/zsh
ENV IN_CONTAINER=1
ENV POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

WORKDIR /home/dev/projects

ENTRYPOINT ["/home/dev/.local/bin/entrypoint.sh"]
CMD ["/bin/zsh"]

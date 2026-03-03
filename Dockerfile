FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# ── Bootstrap (bare minimum for nix to run) ───────────────────────────────────
RUN apt-get update && apt-get install -y \
    build-essential \
    ca-certificates \
    curl \
    git \
    locales \
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
    && printf 'experimental-features = nix-command flakes\nconnect-timeout = 120\nhttp-connections = 1\n' > ~/.config/nix/nix.conf

ENV PATH=/home/dev/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH

# ── nixpkgs channel (downloaded from nixos.org, avoids GitHub CDN inside Docker)
RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs \
    && nix-channel --update

# ── Dev tools via flake ───────────────────────────────────────────────────────
COPY --chown=dev:dev flake.nix /tmp/devenv/flake.nix
RUN NIXPKGS_PATH=$(nix-instantiate --find-file nixpkgs) \
    && nix profile install 'path:/tmp/devenv' \
       --override-input nixpkgs "path:$NIXPKGS_PATH" \
    && nix-collect-garbage -d \
    && rm -rf /tmp/devenv

RUN git lfs install

# ── Powerlevel10k theme (link nix package into oh-my-zsh custom themes) ──────
RUN mkdir -p ~/.config/oh-my-zsh-custom/themes \
    && ln -s ~/.nix-profile/share/zsh-powerlevel10k \
       ~/.config/oh-my-zsh-custom/themes/powerlevel10k

# ── Entrypoint ────────────────────────────────────────────────────────────────
# Dotfiles are mounted as a volume at runtime (/home/dev/.dotfiles).
# The entrypoint symlinks them into place on each container start.
COPY --chmod=755 docker/entrypoint.sh /usr/local/bin/entrypoint.sh

ENV SHELL=/bin/zsh
ENV IN_CONTAINER=1
ENV POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

WORKDIR /home/dev/projects

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/bin/zsh"]

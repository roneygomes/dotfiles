export LC_ALL=en_US.UTF-8
export DISABLE_AUTO_TITLE=true

export PROJECTS_DIR=~/projects

# oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
export plugins=(cp vi-mode git git-extras fzf z)
export ZSH_THEME="powerlevel10k/powerlevel10k"

# nix
if [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi
PATH=$HOME/.nix-profile/bin:$PATH

# bins
PATH=$HOME/.local/bin:$PATH

# node
export NODE_OPTIONS="--max-old-space-size=2048"

# ssh agent (socket created by entrypoint via socat bridge)
export SSH_AUTH_SOCK=/tmp/ssh-auth.sock

# docker
export DOCKER_CONFIG=$HOME/.config/docker
export COMPOSE_BAKE=true

# nx
export NX_CACHE_DIRECTORY=$HOME/.local/nx/cache

# OpenAI Codex
export CODEX_HOME=$HOME/.config/codex

export PATH=$PATH

export LC_ALL=en_US.UTF-8
export DISABLE_AUTO_TITLE=true

export PROJECTS_DIR=~/projects

# oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
export plugins=(cp vi-mode git git-extras fzf z nvm)
export ZSH_THEME="powerlevel10k/powerlevel10k"

# homebrew
PATH=/opt/homebrew/bin:$PATH:

# psql
PATH="/opt/homebrew/opt/libpq/bin:$PATH"

# homebrew
export HOMEBREW_NO_ENV_HINTS=true

# openssl shenanigans
export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@3/include"

[[ $(uname) == Darwin ]] && PKG_CONFIG_PATH="$(brew --prefix)/opt/cyrus-sasl/lib/pkgconfig"
export PKG_CONFIG_PATH

[[ $(uname) == Darwin ]] && OPENSSL_ROOT_DIR="$(brew --prefix)/opt/openssl@3"
export OPENSSL_ROOT_DIR

PATH=/usr/local/opt/openssl@3/bin:$PATH:

# bitwarden SSH agent
SSH_AUTH_SOCK=$HOME/.bitwarden-ssh-agent.sock

# bins
PATH=$HOME/.local/bin:$PATH:

# rancher desktop
PATH=$HOME/.rd/bin:$PATH:

# java
PATH=/opt/homebrew/opt/openjdk/bin:$PATH

# nodejs
export NODE_OPTIONS="--max-old-space-size=2048"

# docker
export DOCKER_CONFIG=$HOME/.config/docker
export COMPOSE_BAKE=true

# gcloud
export CLOUDSDK_PYTHON_SITEPACKAGES=1

# keep at the end
export PATH=$PATH

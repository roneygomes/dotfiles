export LC_ALL=en_US.UTF-8
export DISABLE_AUTO_TITLE=true

# oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
export plugins=(cp vi-mode git git-extras fzf z)
export ZSH_THEME=refined

# homebrew
PATH=/opt/homebrew/bin:$PATH:

# psql
PATH="/opt/homebrew/opt/libpq/bin:$PATH"

# homebrew
export HOMEBREW_NO_ENV_HINTS=true

# openssl shenanigans
export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
export CPPFLAGS="-I/usr/local/opt/openssl@3/include"

PKG_CONFIG_PATH="$(brew --prefix)/opt/cyrus-sasl/lib/pkgconfig"
export PKG_CONFIG_PATH

OPENSSL_ROOT_DIR="$(brew --prefix)/opt/openssl@3"
export OPENSSL_ROOT_DIR

PATH=/usr/local/opt/openssl@3/bin:$PATH:

# bitwarden SSH agent
SSH_AUTH_SOCK=/Users/roney/.bitwarden-ssh-agent.sock

# bins
PATH=$HOME/.local/bin:$PATH:

# rancher desktop
PATH=$HOME/.rd/bin:$PATH:

# asdf
PATH=$HOME/.asdf/shims:$PATH:

# go
PATH=$(asdf where golang)/packages/bin:$PATH:

# rust
. "$(asdf where rust)"/env

# java
export JAVA_HOME=$(asdf where java)

# nodejs
PATH=$(asdf where nodejs)/bin:$PATH:
export NODE_OPTIONS="--max-old-space-size=2048"

# docker
export DOCKER_CONFIG=$HOME/.config/docker
export COMPOSE_BAKE=true

# nx
export NX_CACHE_DIRECTORY=$HOME/.local/nx/cache

# gcloud
export CLOUDSDK_PYTHON_SITEPACKAGES=1

# OpenAI Codex
export CODEX_HOME=$HOME/.config/codex

# keep at the end
export PATH=$PATH

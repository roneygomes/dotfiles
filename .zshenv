export LC_ALL=en_US.UTF-8
export DISABLE_AUTO_TITLE=true

export PROJECTS_DIR=~/projects

# oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
export plugins=(cp vi-mode git git-extras fzf z)
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
SSH_AUTH_SOCK=/Users/roney/.bitwarden-ssh-agent.sock

# bins
PATH=$HOME/.local/bin:$PATH:

# rancher desktop
PATH=$HOME/.rd/bin:$PATH:

# asdf
PATH=$HOME/.asdf/shims:$PATH:

# go
_go="$(asdf where golang 2>/dev/null)"
[[ -n "$_go" ]] && PATH=$_go/packages/bin:$PATH
unset _go

# rust
_rust_env="$(asdf where rust 2>/dev/null)/env"
[[ -f "$_rust_env" ]] && . "$_rust_env"
unset _rust_env

# java
_java="$(asdf where java 2>/dev/null)"
[[ -n "$_java" ]] && export JAVA_HOME=$_java
unset _java

# nodejs
_nodejs="$(asdf where nodejs 2>/dev/null)"
[[ -n "$_nodejs" ]] && PATH=$_nodejs/bin:$PATH
unset _nodejs
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

export LC_ALL=en_US.UTF-8

# oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
export plugins=(helpers vi-mode git git-extras fzf-zsh-plugin z)

# macOS
if [[ "$(uname)" == "Darwin" ]]; then
    # homebrew
    PATH=/opt/homebrew/bin:$PATH
    export HOMEBREW_NO_ENV_HINTS=true

    # openssl shenanigans
    export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
    export CPPFLAGS="-I/usr/local/opt/openssl@3/include"

    export PKG_CONFIG_PATH="$(brew --prefix)/opt/cyrus-sasl/lib/pkgconfig"
    export OPENSSL_ROOT_DIR="$(brew --prefix)/opt/openssl@3"

    PATH=/usr/local/opt/openssl@3/bin:$PATH
fi

# bins
PATH=~/bin:$PATH:
PATH=$HOME/.local/bin:$PATH:

# asdf
PATH=$HOME/.asdf/shims:$PATH:

# go
PATH=$(asdf where golang)/packages/bin:$PATH:

# rust
. $HOME/.cargo/env

export PATH=$PATH

export LC_ALL=en_US.UTF-8

# oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
export plugins=(helpers vi-mode git git-extras fzf-zsh-plugin z)

# macOS
if [[ "$(uname)" == "Darwin" ]]; then
    # homebrew
    PATH=$PATH:/opt/homebrew/bin
    export HOMEBREW_NO_ENV_HINTS=true

    # openssl shenanigans
    export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
    export CPPFLAGS="-I/usr/local/opt/openssl@3/include"

    export PKG_CONFIG_PATH="$(brew --prefix)/opt/cyrus-sasl/lib/pkgconfig"
    export OPENSSL_ROOT_DIR="$(brew --prefix)/opt/openssl@3"

    PATH=$PATH:/usr/local/opt/openssl@3/bin
fi

# bins
PATH=$PATH:~/bin
PATH=$PATH:$HOME/.local/bin

# asdf
PATH=$PATH:$HOME/.asdf/shims

# go
PATH=$PATH:$(asdf where golang)/packages/bin

# rust
. $HOME/.cargo/env

export PATH=$PATH

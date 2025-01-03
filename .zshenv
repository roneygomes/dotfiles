export LC_ALL=en_US.UTF-8
export PROJECTS_DIR=$HOME/Projects/Personal
export INFRA_DIR=$HOME/Projects/Infra

export PATH=$PATH:~/bin
export PATH=$PATH:$PROJECTS_DIR/dotfiles/zsh/bin

# rust
. "$HOME/.cargo/env"

# go
export PATH=$PATH:$HOME/go/bin

if [[ "$(uname)" == "Darwin" ]]; then
    # homebrew
    export PATH=$PATH:/opt/homebrew/bin
    export HOMEBREW_NO_ENV_HINTS=true

    # openssl shenanigans
    export PATH="/usr/local/opt/openssl@3/bin:$PATH"
    export LDFLAGS="-L/usr/local/opt/openssl@3/lib"
    export CPPFLAGS="-I/usr/local/opt/openssl@3/include"

    export PKG_CONFIG_PATH="$(brew --prefix)/opt/cyrus-sasl/lib/pkgconfig"
    export OPENSSL_ROOT_DIR="$(brew --prefix)/opt/openssl@3"

    # find python
    for dir in ~/Library/Python/*/bin; do
	[ -d "$dir" ] && PATH="$dir:$PATH"
    done
    export PATH 
fi

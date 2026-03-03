#!/bin/sh
set -e

if [ ! -S /var/run/docker.sock ]; then
    echo "error: Docker socket not found. Mount it with -v /var/run/docker.sock:/var/run/docker.sock"
    exit 1
fi

sudo chmod 666 /var/run/docker.sock

# ── Dotfiles setup ────────────────────────────────────────────────────────────
# Symlink files from the mounted dotfiles repo (~/.dotfiles) into place.
DOTFILES="$HOME/.dotfiles"
if [ -d "$DOTFILES" ]; then
    # Shell config (container-specific variants live under docker/)
    ln -sf "$DOTFILES/docker/.zshrc"  "$HOME/.zshrc"
    ln -sf "$DOTFILES/docker/.zshenv" "$HOME/.zshenv"

    # Shared dotfiles
    [ -f "$DOTFILES/.p10k.zsh" ] && ln -sf "$DOTFILES/.p10k.zsh" "$HOME/.p10k.zsh"

    # Git config (XDG location)
    if [ -d "$DOTFILES/.config/git" ]; then
        mkdir -p "$HOME/.config"
        ln -sfn "$DOTFILES/.config/git" "$HOME/.config/git"
    fi

    # Scripts
    if [ -d "$DOTFILES/.local/bin" ]; then
        mkdir -p "$HOME/.local/bin"
        for f in "$DOTFILES/.local/bin/"*; do
            [ -f "$f" ] && ln -sf "$f" "$HOME/.local/bin/$(basename "$f")"
        done
    fi
fi

exec "$@"

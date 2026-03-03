#!/bin/sh
set -e

if [ ! -S /var/run/docker.sock ]; then
    echo "error: Docker socket not found. Mount it with -v /var/run/docker.sock:/var/run/docker.sock"
    exit 1
fi

sudo chmod 666 /var/run/docker.sock

# ── SSH setup ─────────────────────────────────────────────────────────────────
# The host ~/.ssh is mounted read-only at ~/.ssh-host. Copy it to ~/.ssh so we
# can set the strict ownership/permissions SSH requires.
if [ -d "$HOME/.ssh-host" ]; then
    mkdir -p "$HOME/.ssh"
    cp -r "$HOME/.ssh-host/." "$HOME/.ssh/"
    chmod 700 "$HOME/.ssh"
    chmod 600 "$HOME/.ssh"/* 2>/dev/null || true
    chmod 644 "$HOME/.ssh"/*.pub 2>/dev/null || true
fi

exec "$@"

#!/bin/sh
set -e

if [ ! -S /var/run/docker.sock ]; then
    echo "error: Docker socket not found. Mount it with -v /var/run/docker.sock:/var/run/docker.sock"
    exit 1
fi

sudo chmod 666 /var/run/docker.sock

# ── SSH setup ─────────────────────────────────────────────────────────────────
# Copy ~/.ssh-host (read-only bind mount) to ~/.ssh with correct permissions.
if [ -d "$HOME/.ssh-host" ]; then
    sudo cp -r "$HOME/.ssh-host/." "$HOME/.ssh/"
    sudo chown -R dev:dev "$HOME/.ssh"
    chmod -R u=rwX,go= "$HOME/.ssh"
    chmod 644 "$HOME/.ssh"/*.pub 2>/dev/null || true
fi

# ── SSH agent bridge ──────────────────────────────────────────────────────────
# Connect to the macOS SSH agent via the TCP bridge started by `devbox start`,
# and expose it locally as a Unix socket that SSH clients expect.
if [ -n "${SSH_AGENT_BRIDGE_PORT:-}" ]; then
    socat "UNIX-LISTEN:/tmp/ssh-auth.sock,fork,reuseaddr,unlink-early" \
          "TCP:host.docker.internal:${SSH_AGENT_BRIDGE_PORT}" &
fi

exec "$@"

#!/bin/sh
set -e

# ── Nix profile repair ───────────────────────────────────────────────────────
# The home directory lives on a persistent volume, so the nix profile symlink
# may point to a store path from a previous image build. Re-link if broken.
NIX_PROFILE="$HOME/.nix-profile"
if [ -L "$NIX_PROFILE" ] && [ ! -e "$NIX_PROFILE/bin" ]; then
    PROFILE=$(find /nix/store -maxdepth 1 -name "*-profile" -type d 2>/dev/null | head -1)
    if [ -n "$PROFILE" ] && [ -d "$PROFILE/bin" ]; then
        rm -f "$HOME/.local/state/nix/profiles"/profile*
        ln -s "$PROFILE" "$HOME/.local/state/nix/profiles/profile-1-link"
        ln -s profile-1-link "$HOME/.local/state/nix/profiles/profile"
    fi
fi

export PATH="$NIX_PROFILE/bin:$PATH"

if [ ! -S /var/run/docker.sock ]; then
    echo "error: Docker socket not found. Mount it with -v /var/run/docker.sock:/var/run/docker.sock"
    exit 1
fi

sudo chmod 666 /var/run/docker.sock

# ── SSH setup ─────────────────────────────────────────────────────────────────
# Start fresh — the volume may have stale files from a previous image.
# Copy only known_hosts from the host; keys come via the agent bridge.
rm -rf "$HOME/.ssh"
if [ -d "$HOME/.ssh-host" ]; then
    mkdir -p "$HOME/.ssh"
    cp "$HOME/.ssh-host"/known_hosts* "$HOME/.ssh/" 2>/dev/null || true
    chown -R "$(whoami):$(id -gn)" "$HOME/.ssh"
    chmod 700 "$HOME/.ssh"
    chmod 644 "$HOME/.ssh"/known_hosts* 2>/dev/null || true
fi

# ── SSH agent bridge ──────────────────────────────────────────────────────────
# Connect to the macOS SSH agent via the TCP bridge started by `devbox start`,
# and expose it locally as a Unix socket that SSH clients expect.
if [ -n "${SSH_AGENT_BRIDGE_PORT:-}" ]; then
    socat "UNIX-LISTEN:/tmp/ssh-auth.sock,fork,reuseaddr,unlink-early" \
          "TCP:host.docker.internal:${SSH_AGENT_BRIDGE_PORT}" &
fi

# ── 1Password daemon bridge ─────────────────────────────────────────────────
if [ -n "${OP_AGENT_BRIDGE_PORT:-}" ]; then
    OP_DAEMON_SOCK="/tmp/op-daemon.sock"
    socat "UNIX-LISTEN:${OP_DAEMON_SOCK},fork,reuseaddr,unlink-early" \
          "TCP:host.docker.internal:${OP_AGENT_BRIDGE_PORT}" &

    mkdir -p -m 700 "$HOME/.config/op"
    cat > "$HOME/.config/op/config" <<OPEOF
{
  "daemon": {
    "socketPath": "${OP_DAEMON_SOCK}"
  }
}
OPEOF
    chmod 600 "$HOME/.config/op/config"
fi

exec "$@"

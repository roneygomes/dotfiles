#!/bin/sh
set -e

# ── Nix profile repair ───────────────────────────────────────────────────────
# The home directory lives on a persistent volume, so the nix profile symlink
# may point to a store path from a previous image build. Re-link if broken.
NIX_PROFILE="$HOME/.nix-profile"
if [ -L "$NIX_PROFILE" ] && [ ! -e "$NIX_PROFILE/bin" ]; then
    PROFILE=$(find /nix/store -maxdepth 1 -name "*-profile" -type d 2>/dev/null | head -1)
    if [ -n "$PROFILE" ] && [ -d "$PROFILE/bin" ]; then
        mkdir -p "$HOME/.local/state/nix/profiles"
        rm -f "$HOME/.local/state/nix/profiles"/profile*
        ln -s "$PROFILE" "$HOME/.local/state/nix/profiles/profile-1-link"
        ln -s profile-1-link "$HOME/.local/state/nix/profiles/profile"
        ln -sf "$HOME/.local/state/nix/profiles/profile" "$NIX_PROFILE"
    fi
fi

# ── Fix stale home-path symlinks ──────────────────────────────────────────────
# The volume may contain symlinks targeting a previous USER_HOME (e.g. /home/dev
# when the current HOME is /Users/roney). Find broken symlinks whose target
# starts with a home-like prefix and rebase them to the current $HOME.
find "$HOME" -xdev -type l ! -exec test -e {} \; -print 2>/dev/null \
| while IFS= read -r link; do
    target=$(readlink "$link")
    # Match absolute paths starting with /home/<user>/... or /Users/<user>/...
    case "$target" in
        /home/*/*|/Users/*/*)
            # Strip the old home prefix (first 3 path components: /home/user or /Users/user)
            old_home=$(echo "$target" | cut -d/ -f1-3)
            suffix=${target#"$old_home"}
            new_target="$HOME$suffix"
            if [ -e "$new_target" ] || [ -L "$new_target" ]; then
                ln -sf "$new_target" "$link"
            fi
            ;;
    esac
done

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

# ── GitHub CLI ────────────────────────────────────────────────────────────────
# The bind mount for .devbox-token may cause Docker to create ~/.config/gh/
# as root. Fix ownership so gh can write hosts.yml if needed.
if [ -d "$HOME/.config/gh" ]; then
    sudo chown "$(whoami):$(id -gn)" "$HOME/.config/gh"
fi

# ── npm cache permissions ─────────────────────────────────────────────────────
# npx bin stubs on Docker volumes may lose the execute bit.
chmod -R u+x "$HOME/.npm/_npx" 2>/dev/null || true

exec "$@"

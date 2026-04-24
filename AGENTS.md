# AGENTS.md

This file provides guidance to AI agents when working with code in this repository.

## What This Is

Personal dotfiles repository managing shell configuration, application settings, and custom scripts. Uses GNU Stow to symlink files from this repo to `$HOME`.

## Deploying Changes

After modifying any configuration file:
```sh
make stow
```

This creates/updates symlinks from `$HOME` to files in this repository. The repository structure mirrors `$HOME` directory layout.

`make stow` runs stow with `--no-folding` so directories like `~/.config` stay as real dirs with per-file symlinks — never wholesale-symlinked into the repo. Always go through `make stow`; calling `stow` directly without `--no-folding` will refold and cause apps to write their state into the repo.

## Repository Organization

**Configuration files:**
- `.zshrc` - Shell configuration, functions, aliases
- `.zshenv` - Environment variables, PATH setup
- `.config/` - Application-specific configs (git, gh, emacs, iterm2, etc.)
- `.claude-profiles/<profile>/settings.json` - Per-profile Claude Code config (one per profile, tracked in git)
- `~/.claude/settings.local.json` - Personal overrides for env, secrets, machine-specific paths (real file, not tracked)

**Executable scripts:**
- `.local/bin/` - Custom shell scripts and utilities

**Package management:**
- `Brewfile` - Homebrew package definitions

## Making Changes

**Adding a new script:**
1. Create executable in `.local/bin/`
2. Run `make stow`
3. Script becomes available in `$PATH`

**Modifying shell config:**
1. Edit `.zshrc` or `.zshenv`
2. Run `make stow`
3. Source with `source ~/.zshrc` or restart shell

**Adding application config:**
1. Create/modify file in `.config/<app-name>/`
2. Run `make stow`
3. Config symlinked to `~/.config/<app-name>/`

**Installing new packages:**
1. Add to `Brewfile`
2. Run `brew bundle install`

**Modifying Claude Code config:**
- Edit the profile you use: `.claude-profiles/personal/settings.json` or `.claude-profiles/work/settings.json`. `.zshenv` selects the profile by hostname (personal on `bluestar`, work elsewhere) via `CLAUDE_CONFIG_DIR`.
- Machine-local overrides (env vars, paths that shouldn't be committed): edit `~/.claude/settings.local.json` directly — that file is not tracked or stowed.

**Granting cco sandbox access to new paths:**
- `cco` is a sandbox wrapper for Claude Code (installed at `~/.local/share/cco/cco`, symlinked to `/usr/local/bin/cco`)
- The `cco` alias in `.zshrc` adds `--add-dir <path>:rw` flags for directories that need write access inside the sandbox
- To grant access to a new directory, add another `--add-dir <path>:rw` to the alias in `.zshrc`
- Do NOT modify the profile `settings.json` for sandbox path access — that's for Claude Code permissions, not cco sandbox rules

## Testing Changes

Test configuration changes before committing by sourcing or restarting affected applications. Stow creates symlinks, so changes to files in this repo immediately affect the live environment.

## Commit Messages

When creating commits, use the Conventional Commits format:

`type(scope): short description`

Examples:
- `feat(zsh): add fzf history keybinding`
- `fix(git): correct rebase alias behavior`
- `chore(brew): update Brewfile packages`

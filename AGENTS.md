# AGENTS.md

This file provides guidance to AI agents when working with code in this repository.

## What This Is

Personal dotfiles repository managing shell configuration, application settings, and custom scripts. Uses GNU Stow to symlink files from this repo to `$HOME`.

## Deploying Changes

After modifying any configuration file:
```sh
stow --verbose 1 --target $HOME .
```

This creates/updates symlinks from `$HOME` to files in this repository. The repository structure mirrors `$HOME` directory layout.

## Repository Organization

**Configuration files:**
- `.zshrc` - Shell configuration, functions, aliases
- `.zshenv` - Environment variables, PATH setup
- `.config/` - Application-specific configs (git, gh, emacs, iterm2, etc.)
- `.claude/settings.json` - Claude Code permissions (tracked in git)
- `.claude/settings.local.json` - Local permission overrides (git-ignored)

**Executable scripts:**
- `.local/bin/` - Custom shell scripts and utilities

**Package management:**
- `Brewfile` - Homebrew package definitions

## Making Changes

**Adding a new script:**
1. Create executable in `.local/bin/`
2. Run `stow --verbose 1 --target $HOME .`
3. Script becomes available in `$PATH`

**Modifying shell config:**
1. Edit `.zshrc` or `.zshenv`
2. Run `stow --verbose 1 --target $HOME .`
3. Source with `source ~/.zshrc` or restart shell

**Adding application config:**
1. Create/modify file in `.config/<app-name>/`
2. Run `stow --verbose 1 --target $HOME .`
3. Config symlinked to `~/.config/<app-name>/`

**Installing new packages:**
1. Add to `Brewfile`
2. Run `brew bundle install`

**Modifying Claude Code permissions:**
- Global: Edit `.claude/settings.json`, commit changes
- Project-specific: Edit `.claude/settings.local.json` (not committed)

## Testing Changes

Test configuration changes before committing by sourcing or restarting affected applications. Stow creates symlinks, so changes to files in this repo immediately affect the live environment.

## Commit Messages

When creating commits, use the Conventional Commits format:

`type(scope): short description`

Examples:
- `feat(zsh): add fzf history keybinding`
- `fix(git): correct rebase alias behavior`
- `chore(brew): update Brewfile packages`

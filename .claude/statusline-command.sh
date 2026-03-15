#!/bin/sh
input=$(cat)

user=$(whoami)
host=$(hostname -s)
cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // ""')
model=$(echo "$input" | jq -r '.model.display_name // ""')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Shorten home directory to ~
home="$HOME"
short_cwd=$(echo "$cwd" | sed "s|^$home|~|")

# Git branch (skip optional locks to avoid blocking)
git_branch=""
if git -C "$cwd" rev-parse --git-dir > /dev/null 2>&1; then
  git_branch=$(GIT_OPTIONAL_LOCKS=0 git -C "$cwd" symbolic-ref --short HEAD 2>/dev/null)
fi

# Build context usage indicator
ctx_info=""
if [ -n "$used" ]; then
  used_int=$(printf "%.0f" "$used")
  ctx_info=" ctx: ${used_int}%"
fi

# Build git segment
git_segment=""
if [ -n "$git_branch" ]; then
  git_segment=$(printf " \x1b[33m(%s)\x1b[0m" "$git_branch")
fi

printf "\x1b[32m%s@%s\x1b[0m \x1b[34m%s\x1b[0m%s \x1b[90m| %s%s\x1b[0m" \
  "$user" "$host" "$short_cwd" "$git_segment" "$model" "$ctx_info"

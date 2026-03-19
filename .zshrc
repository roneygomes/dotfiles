# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#!/bin/zsh

# ==============================================================================
# OH-MY-ZSH
# ==============================================================================

ZSH=$HOME/.oh-my-zsh
source "$ZSH"/oh-my-zsh.sh

# ==============================================================================
# SHELL OPTIONS
# ==============================================================================

unsetopt prompt_subst
setopt prompt_subst

autoload -U colors
colors

# ==============================================================================
# PATH & ENVIRONMENT
# ==============================================================================

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/roney.gomes/.rd/bin:$PATH"

# ==============================================================================
# EXTERNAL SOURCES
# ==============================================================================

# 1Password plugins
if [[ -f "$HOME"/.config/op/plugins.sh ]]; then
	source "$HOME"/.config/op/plugins.sh
fi

# Secret stuff that can't be versioned
if [[ -f "$HOME"/.private ]]; then
	source "$HOME"/.private
fi

# Google Cloud SDK
if [ -f '/opt/homebrew/share/google-cloud-sdk/path.zsh.inc' ]; then
	. '/opt/homebrew/share/google-cloud-sdk/path.zsh.inc'
fi

if [ -f '/opt/homebrew/share/google-cloud-sdk/completion.zsh.inc' ]; then
	. '/opt/homebrew/share/google-cloud-sdk/completion.zsh.inc'
fi

# ==============================================================================
# COMPLETIONS
# ==============================================================================

# Append completions to fpath
fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)

# Initialise completions with ZSH's compinit
autoload -Uz compinit && compinit

# ==============================================================================
# TMUX AUTO-ATTACH
# ==============================================================================

if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" ]]; then
  exec tmux new-session -A -s main
fi

# ==============================================================================
# KEY BINDINGS
# ==============================================================================


# ==============================================================================
# ALIASES
# ==============================================================================

# Local IP address
# alias ip="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"

# Brew bundle install
alias bbi="brew update && \
    brew bundle install --cleanup --file=~/.config/Brewfile && \
    brew upgrade"

# Milliseconds since epoch
alias epoch="python3 -c 'import time; print(int(time.time() * 1000))'"

alias prettier="npx prettier"

# Edit Brewfile
alias brewfile="vim ~/.config/Brewfile"

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Desktop notification (macOS)
notify() {
	osascript -e "display notification \"$1\" with title \"$2\""
}

# Generate UUID and copy to clipboard
uuid() {
	uuid_string=$(uuidgen | awk '{print tolower($0)}')
	printf "%s" "$uuid_string" | pbcopy
	echo "$uuid_string"
}

# Run command in the background in a tmux session.
# Usage: run_bg session_name "command"
tmux_bg() {
    tmux new-session -d -s "$1" "$2"
    echo "Task '$2' is running in tmux session: $1."
}

# Quote lines from file or stdin, adding commas (except last line)
quote_lines() {
	local quote_char="'"
	local input_source

	# Parse arguments
	while [[ $# -gt 0 ]]; do
		case $1 in
			-d|--double)
				quote_char='"'
				shift
				;;
			-s|--single)
				quote_char="'"
				shift
				;;
			-*)
				echo "Usage: quote_lines [-d|--double|-s|--single] [file]" >&2
				return 1
				;;
			*)
				input_source="$1"
				shift
				;;
		esac
	done

	# Read from file or stdin
	local lines=()

	if [[ -n "$input_source" ]]; then
		if [[ ! -f "$input_source" ]]; then
			echo "Error: File '$input_source' not found" >&2
			return 1
		fi

		while IFS= read -r line; do
			lines+=("$line")
		done < "$input_source"
	else
		while IFS= read -r line; do
			lines+=("$line")
		done
	fi

	# Quote lines and add commas
	local count=${#lines[@]}

	for ((i=1; i<=count; i++)); do
		if [[ $i -lt $count ]]; then
			echo "${quote_char}${lines[$i]}${quote_char},"
		else
			echo "${quote_char}${lines[$i]}${quote_char}"
		fi
	done
}

# Append a timestamped entry to today's markdown daily note in iCloud Drive
quicknote() {
  if [ -z "$1" ]; then
    echo "Usage: quicknote <your note text>"
    return 1
  fi

  local NOTES_DIR="$HOME/Library/Mobile Documents/com~apple~CloudDocs/Notes"
  local CURRENT_DATE=$(date +"%Y-%m-%d")
  local CURRENT_TIME=$(date +"%H:%M")
  local NOTE_FILE="$NOTES_DIR/$CURRENT_DATE.md"

  mkdir -p "$NOTES_DIR"

  if [ ! -f "$NOTE_FILE" ]; then
    echo "# $CURRENT_DATE" > "$NOTE_FILE"
  fi

  echo "- **$CURRENT_TIME** $*" >> "$NOTE_FILE"
}

# Open today's daily note in Vim
dailynote() {
  local NOTES_DIR="$HOME/Library/Mobile Documents/com~apple~CloudDocs/Notes"
  local NOTE_FILE="$NOTES_DIR/$(date +"%Y-%m-%d").md"
  mkdir -p "$NOTES_DIR"
  vim "$NOTE_FILE"
}

# Browse notes with fzf and open selection in Vim
notelist() {
  local NOTES_DIR="$HOME/Library/Mobile Documents/com~apple~CloudDocs/Notes"
  local file=$(ls -1t "$NOTES_DIR"/*.md 2>/dev/null | xargs -I{} basename {} | fzf --height 80% --reverse --prompt='Select note: ' --preview "cat '$NOTES_DIR/{}'")

  [ -n "$file" ] && vim "$NOTES_DIR/$file"
}

# Search notes content with fzf and open selection in Vim
notesearch() {
  local NOTES_DIR="$HOME/Library/Mobile Documents/com~apple~CloudDocs/Notes"
  local result=$(rg --line-number --no-heading . "$NOTES_DIR" 2>/dev/null | sed "s|$NOTES_DIR/||" | fzf --height 40% --reverse --prompt='Search notes: ')
  if [ -n "$result" ]; then
    local file=$(echo "$result" | cut -d: -f1)
    local line=$(echo "$result" | cut -d: -f2)
    vim "+$line" "$NOTES_DIR/$file"
  fi
}

# ==============================================================================
# GIT FUNCTIONS
# ==============================================================================

# Git branch selector with fzf (sorted by recency)
git_branch_selector() {
	local branch=$(git for-each-ref --sort=-committerdate --format='%(refname:short)' refs/heads | fzf --height 40% --reverse --prompt='Select branch: ')
	[ -n "$branch" ] && git checkout "$branch"
}

# Delete local branches older than one week with deleted remote branches
git_cleanup_stale_branches() {
	local current_branch=$(git branch --show-current)
	local one_week_ago=$(date -v-7d +%s 2>/dev/null || date -d "7 days ago" +%s)
	local deleted_count=0

	# Fetch latest remote refs to check what exists
	git fetch --prune --quiet 2>/dev/null

	# Get all local branches except current
	local branches=($(git branch --format='%(refname:short)' | grep -v "^$current_branch$"))

	for branch in "${branches[@]}"; do
		# Get last commit timestamp for this branch
		local last_commit=$(git log -1 --format=%ct "$branch" 2>/dev/null)
		[[ -z "$last_commit" ]] && continue

		# Check if branch is older than one week
		if [[ $last_commit -lt $one_week_ago ]]; then
			# Check if remote branch exists
			if ! git ls-remote --heads origin "$branch" 2>/dev/null | grep -q "$branch"; then
				echo "Deleting stale branch: $branch (last updated: $(date -r $last_commit +%Y-%m-%d 2>/dev/null || date -d @$last_commit +%Y-%m-%d))"
				if git branch -D "$branch" 2>/dev/null; then
					((deleted_count++))
				fi
			fi
		fi
	done

	echo "Cleaned up $deleted_count stale branch(es)"
}



# ==============================================================================
# EMACS VTERM INTEGRATION
# ==============================================================================

vterm_printf() {
	if [ -n "$TMUX" ] &&
		{ [ "${TERM%%-*}" = "tmux" ] ||
			[ "${TERM%%-*}" = "screen" ]; }; then
		# Tell tmux to pass the escape sequences through
		printf "\ePtmux;\e\e]%s\007\e\\" "$1"
	elif [ "${TERM%%-*}" = "screen" ]; then
		# GNU screen (screen, screen-256color, screen-256color-bce)
		printf "\eP\e]%s\007\e\\" "$1"
	else
		printf "\e]%s\e\\" "$1"
	fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
	alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# ==============================================================================
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# iTerm2 shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh" || true

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# place this after nvm initialization!
autoload -U add-zsh-hook

load-nvmrc() {
  local nvmrc_path
  nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version
    nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$(nvm version)" ]; then
      nvm use
    fi
  elif [ -n "$(PWD=$OLDPWD nvm_find_nvmrc)" ] && [ "$(nvm version)" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}

add-zsh-hook chpwd load-nvmrc
load-nvmrc

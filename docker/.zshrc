# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# ==============================================================================
# OH-MY-ZSH
# ==============================================================================

ZSH=$HOME/.nix-profile/share/oh-my-zsh
source "$ZSH"/oh-my-zsh.sh

# ==============================================================================
# SHELL OPTIONS
# ==============================================================================

unsetopt prompt_subst
setopt prompt_subst

autoload -U colors
colors

# ==============================================================================
# COMPLETIONS
# ==============================================================================

autoload -Uz compinit && compinit

# ==============================================================================
# ALIASES
# ==============================================================================

alias epoch="python3 -c 'import time; print(int(time.time() * 1000))'"
alias prettier="npx prettier"

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Generate UUID
uuid() {
	uuidgen | awk '{print tolower($0)}'
}

# Quote lines from file or stdin, adding commas (except last line)
quote_lines() {
	local quote_char="'"
	local input_source

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

	local count=${#lines[@]}

	for ((i=1; i<=count; i++)); do
		if [[ $i -lt $count ]]; then
			echo "${quote_char}${lines[$i]}${quote_char},"
		else
			echo "${quote_char}${lines[$i]}${quote_char}"
		fi
	done
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

	git fetch --prune --quiet 2>/dev/null

	local branches=($(git branch --format='%(refname:short)' | grep -v "^$current_branch$"))

	for branch in "${branches[@]}"; do
		local last_commit=$(git log -1 --format=%ct "$branch" 2>/dev/null)
		[[ -z "$last_commit" ]] && continue

		if [[ $last_commit -lt $one_week_ago ]]; then
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

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

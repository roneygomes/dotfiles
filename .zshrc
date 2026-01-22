#!/bin/zsh

ZSH=$HOME/.oh-my-zsh

source "$ZSH"/oh-my-zsh.sh

# Custom function to display project and worktree info
project_worktree_info() {
    # Check if we're in a git worktree with a bare repository
    if git rev-parse --is-inside-work-tree &>/dev/null; then
        local git_common_dir=$(git rev-parse --git-common-dir 2>/dev/null)
        
        # Check if this is a bare-cloned project (has .bare directory)
        if [ -n "$git_common_dir" ]; then
            local bare_dir=$(cd "$git_common_dir" 2>/dev/null && pwd)
            
            if [ "$(basename "$bare_dir")" = ".bare" ]; then
                # Get project name (parent directory of .bare)
                local project_root=$(dirname "$bare_dir")
                local project_name=$(basename "$project_root")
                
                # Get worktree name (current worktree directory name)
                local git_top_level=$(git rev-parse --show-toplevel 2>/dev/null)
                local worktree_name=$(basename "$git_top_level")
                
                # Only show if we're in a worktree (not at project root)
                if [ "$project_root" != "$git_top_level" ]; then
                    echo "%F{cyan}[$project_name/%F{green}$worktree_name%F{cyan}]%f "
                fi
            fi
        fi
    fi
}

# Git branch and status info
git_prompt_info_inline() {
    if git rev-parse --is-inside-work-tree &>/dev/null; then
        local branch=$(git symbolic-ref --short HEAD 2>/dev/null || git describe --tags --exact-match 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
        local dirty=$(git diff --quiet --ignore-submodules HEAD 2>/dev/null; [ $? -eq 1 ] && echo "*")
        echo "%F{8}git:$branch$dirty%f "
    fi
}

# Override the precmd to disable the multi-line output from refined theme
precmd() {
    # Empty function to override the theme's precmd
}

# Disable the theme's automatic prompt updates
unsetopt prompt_subst
setopt prompt_subst

# disable fzf ctrl-r binding (atuin will rebind ctrl-r at the end)
bindkey -r '^R'

# Prevent fzf from binding ctrl-r if it loads later
export FZF_CTRL_R_OPTS=''

# 1Password plugins
if [[ -f "$HOME"/.config/op/plugins.sh ]]; then
	source "$HOME"/.config/op/plugins.sh
fi

# secret stuff that can't be versioned
if [[ -f "$HOME"/.private ]]; then
	source "$HOME"/.private
fi

# misc
autoload -U colors
colors

# my local ip address
alias ip="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"

# brew bundle install
alias bbi="brew update && \
    brew bundle install --cleanup --file=~/.config/Brewfile && \
    brew upgrade"

# miliseconds since epoch
alias epoch="python3 -c 'import time; print(int(time.time() * 1000))'"
alias prettier="npx prettier"

# desktop notification
notify() {
	osascript -e "display notification \"$1\" with title \"$2\""
}

uuid() {
    uuid_string=$(uuidgen | awk '{print tolower($0)}')
    printf "%s" "$uuid_string" | pbcopy
    echo "$uuid_string"
}

# git branch selector with fzf (sorted by recency)
git_branch_selector() {
    local branch=$(git for-each-ref --sort=-committerdate --format='%(refname:short)' refs/heads | fzf --height 40% --reverse --prompt='Select branch: ')
    [ -n "$branch" ] && git checkout "$branch"
}

# delete local branches older than one week with deleted remote branches
git_cleanup_stale_branches() {
	local current_branch=$(git branch --show-current)
	local one_week_ago=$(date -v-7d +%s 2>/dev/null || date -d "7 days ago" +%s)
	local deleted_count=0
	
	# fetch latest remote refs to check what exists
	git fetch --prune --quiet 2>/dev/null
	
	# get all local branches except current
	local branches=($(git branch --format='%(refname:short)' | grep -v "^$current_branch$"))
	
	for branch in "${branches[@]}"; do
		# get last commit timestamp for this branch
		local last_commit=$(git log -1 --format=%ct "$branch" 2>/dev/null)
		[[ -z "$last_commit" ]] && continue
		
		# check if branch is older than one week
		if [[ $last_commit -lt $one_week_ago ]]; then
			# check if remote branch exists
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

# quote lines from file or stdin, adding commas (except last line)
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

# emacs vterm
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

# append completions to fpath
fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)

# initialise completions with ZSH's compinit
autoload -Uz compinit && compinit

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/roney.gomes/.rd/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/opt/homebrew/share/google-cloud-sdk/path.zsh.inc' ]; then . '/opt/homebrew/share/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/opt/homebrew/share/google-cloud-sdk/completion.zsh.inc' ]; then . '/opt/homebrew/share/google-cloud-sdk/completion.zsh.inc'; fi

# initialise atuin (shell history management) without the up arrow key binding
eval "$(atuin init zsh --disable-up-arrow)"

# Override PROMPT to include project/worktree info on a single line
PROMPT='$(project_worktree_info)$(git_prompt_info_inline)%(?.%F{magenta}.%F{red})❯%f '

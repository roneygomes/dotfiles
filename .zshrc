#!/bin/zsh

ZSH=$HOME/.oh-my-zsh

source "$ZSH"/oh-my-zsh.sh

# 1Password plugins
if [[ -f "$HOME"/.config/op/plugins.sh ]]; then
    source "$HOME"/.config/op/plugins.sh
fi

# secret stuff that can't be versioned
if [[ -f "$HOME"/.private ]]; then
    source "$HOME"/.private
fi

# misc
autoload -U colors; colors

# my local ip address
alias ip="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"

# brew bundle install
alias bbi="brew update && \
    brew bundle install --cleanup --file=~/.config/Brewfile && \
    brew upgrade"

# miliseconds since epoch
alias epoch="python3 -c 'import time; print(int(time.time() * 1000))'"

# desktop notification
notify() {
    osascript -e "display notification \"$1\" with title \"$2\""
}

uuid() {
  uuid_string=$(uuidgen | awk '{print tolower($0)}')
  printf "%s" "$uuid_string" | pbcopy
  echo "$uuid_string"
}

# emacs vterm
vterm_printf() {
    if [ -n "$TMUX" ] \
	&& { [ "${TERM%%-*}" = "tmux" ] \
	    || [ "${TERM%%-*}" = "screen" ]; }; then
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

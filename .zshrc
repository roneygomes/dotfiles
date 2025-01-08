#!/bin/zsh

# oh-my-zsh
export ZSH_DISABLE_COMPFIX=true
export ZSH_THEME="lambda"
export plugins=(helpers vi-mode git git-extras fzf-zsh-plugin z rust asdf)

ZSH=$HOME/.oh-my-zsh

source "$ZSH"/oh-my-zsh.sh
source "$HOME"/.config/op/plugins.sh

if [[ -f "$HOME"/.private ]]; then
    source "$HOME"/.private
fi

# misc
autoload -U colors; colors

alias reload="source ~/.zshrc"
alias ip="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"

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

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/roney.gomes/.rd/bin:$PATH"

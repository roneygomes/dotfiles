alias alert-finished="osascript -e 'display notification \"Finished\" with title \"Alert\"'"
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"

alias myip="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"

FZF_DEFAULT_COMMAND='rg --files --no-ignore-vcs --hidden'
